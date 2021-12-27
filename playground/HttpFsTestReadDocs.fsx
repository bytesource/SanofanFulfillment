#r "nuget: Hopac"
#r "nuget: Http.fs"
#r "nuget: FsToolkit.ErrorHandling"
#r "nuget: Thoth.Json.Net"
// #r "nuget: FsToolkit.ErrorHandling.JobResult"

open System
open Hopac
open HttpFs.Client
open FsToolkit.ErrorHandling
open Thoth.Json.Net


type DomainError = 
    | Decode of string
    | Validation of string
    | Web of string


type Creds = {
    Key: string
    Secret: string
    Url: string
}


type ProductId =  private { Value: int } with
    static member op_Explicit(productId) = productId.Value
    override this.ToString() = this.Value |> string

// Domain implementation
module ProductId = 
    // In my code, the creation of ProductId cannot fail, 
    // but many other fields return Result<value> upon creation.
    let create id = 
        if id > 0 then 
            Ok { ProductId.Value = id }
        else
            DomainError.Validation "ProductId must be larger than zero" |> Error
            // Error [ "ProductId must be larger than zero" ]


let credentials = {
    Key = "ck_808e7e7592b0ca7ae05056af1634b865ea12038a"
    Secret = "cs_64d1a37a8f28a72dd23383626a376a2222678827"
    Url = "https://woocommerce-130476-456889.cloudwaysapps.com/wp-json/wc/v3"
}

let basicAuth creds =
    $"{creds.Key}:{creds.Secret}"
    |> Seq.map byte
    |> Array.ofSeq
    |> Convert.ToBase64String
    |> sprintf "Basic %s"


let request creds endpoint queryParams =
    let basicAuthValue = basicAuth creds
    let finalUrl = $"{creds.Url}{endpoint}" 

    printfn $"Final URL: {finalUrl}"

    let addQueryParams queryParamsOption (request: Request) = 
        let queryParams = 
            queryParamsOption
            |> Option.defaultValue Seq.empty

        queryParams
        |> Seq.fold (fun req (key, value) -> req |> Request.queryStringItem key value) request
            

    Request.createUrl HttpMethod.Get finalUrl
    |> Request.setHeader (Custom ("Authorization", basicAuthValue))
    |> addQueryParams queryParams


let getProductPage creds qParams productId =
    let endpoint = $"/products/{productId}" 

    request creds endpoint qParams


// https://fsharp.slack.com/archives/C10DLSHQV/p1603059320303200
let tryGetResponseAsync =
    HttpFs.Client.tryGetResponse
    >> Hopac.Alt.afterFun Choice.toResult
    >> Hopac.Job.toAsync


let tryGetProductAsync = 
    tryGetResponseAsync
    >> AsyncResult.mapError (fun ex -> DomainError.Web ex.Message)


let getProduct id = asyncResult {
    
        use! response = 
           ProductId.create id
           |> AsyncResult.ofResult
           |> AsyncResult.bind (getProductPage credentials None >> tryGetProductAsync)

        let! getBodyAsync = 
            response
            |> Response.readBodyAsString
            |> Job.toAsync

        let errorMessageDecoder : Decoder<string> = 
            Decode.at [ "message" ] Decode.string 

        let errorMessageFromJson json = 
            Decode.fromString errorMessageDecoder json
            |> Result.mapError DomainError.Web

        match response.statusCode with
        | code when code < 300 && code >= 200 -> 
            return getBodyAsync

        | _  -> 
            match errorMessageFromJson getBodyAsync with
            | Error err -> return! Error err
            // We get 'Ok' if the the error message was decoded successfully,
            // but we need 'Error', because fetching the product information has failed.
            | Ok msg -> 
                let extendedMsg = $"Product Id {id}: {msg}"
            
                return! Error (DomainError.Web extendedMsg)
}



let runIt = 
    getProduct 8
    |> Async.RunSynchronously



// TODO: Where is async?

// PROBLEM:
// Error Response returns as Result.Ok:
// FIXME: Need to deserialize JSON in order to get the error message out!
(*
// Endpoint not found: Status code: Ok 404
{
    "code": "rest_no_route",
    "message": "No route was found matching the URL and request method.",
    "data": {
        "status": 404
    }
}

// Product ID not found: Status code Ok 404

{
    "code":"woocommerce_rest_product_invalid_id",
    "message":"Invalid ID.",
    "data":{
        "status":404
    }
}
*)

