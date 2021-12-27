#r "nuget: Hopac"
#r "nuget: Http.fs"
#r "nuget: FsToolkit.ErrorHandling"

open System
open Hopac
open HttpFs.Client


type DomainError = 
    | Decode of string
    | Validation of string
    | Web of string


type Creds = {
    Key: string
    Secret: string
    Url: string
}


type ProductId = private { Value: int } with
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
            Error "ProductId must be larger than zero"
            // Error [ "ProductId must be larger than zero" ]


let testStoreCredentials = {
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


let getPage creds endpoint =
    let basicAuthValue = basicAuth creds
    let finalUrl = $"{creds.Url}{endpoint}" 

    printfn $"Final URL: {finalUrl}"

    Request.createUrl HttpMethod.Get finalUrl
    |> Request.setHeader (Custom ("Authorization", basicAuthValue))
    |> Request.responseAsString
    |> run


let getProductPage creds productId =
    let endpoint = $"/products/{productId}" 

    getPage creds endpoint


let getProduct8 = 
    ProductId.create 777
    |> Result.map (getProductPage testStoreCredentials)
    |> function   
        | Ok response -> $"Ok: {response}"
        | Error err -> $"Error: {err}"

// TODO: Where is async?

// PROBLEM:
// Error Response returns as Result.Ok:
// FIXME: Need to deserialize JSON in order to get the error message out!
(*
// Endpoint not found:
{
    "code": "rest_no_route",
    "message": "No route was found matching the URL and request method.",
    "data": {
        "status": 404
    }
}

// Product ID not found:

{
    "code":"woocommerce_rest_product_invalid_id",
    "message":"Invalid ID.",
    "data":{
        "status":404
    }
}
*)
