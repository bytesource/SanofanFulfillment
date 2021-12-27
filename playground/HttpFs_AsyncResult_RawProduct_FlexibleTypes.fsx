#r "nuget: Thoth.Json.Net"
#r "nuget: FsToolkit.ErrorHandling"
#r "nuget: Hopac"
#r "nuget: Http.fs"


// Modules
// Helpers
// Types with create and value
// 

open System
open System.IO

open Hopac
open HttpFs.Client
open Thoth.Json.Net
open FsToolkit.ErrorHandling

// Onyx
open System.Net.Http
open System.Text // Encode key and secret to base64 string


// ==================================
// Domain Types
// ==================================

type DomainError = 
    | Decode of string
    | Validation of string
    | Web of string


type Creds = {
    Key: string
    Secret: string
    Url: string
}

type QueryParams = seq<QueryStringName * QueryStringValue>

type ProductId = private { Value: int } with
    static member op_Explicit(productId) = productId.Value
    override  this.ToString() = this.Value |> string

type OrderId = private { Value: int} with
    static member op_Explicit(orderId) = orderId.Value


// Data from a JSON body of an order webhook.
type RawLineItem = { 
    Id: ProductId
    Name: string
}


type RawProduct = {
    Data: RawLineItem
    ShippingRequired: bool
}


// type CheckShippingRequired = ProductId -> Async<Result<bool,DomainError>>


// type CreateRawProduct = 
//     CheckShippingRequired // Dependency
//       -> RawLineItem          // Input
//       -> Async<Result<RawProduct,DomainError>>  // Output


type RawOrder = {
    OrderId: OrderId
    LineItems: RawProduct list
}

// ======================================
// Dependencies
// ======================================

[<Interface>]
type ILogger = 
    abstract Message : string -> unit


[<Interface>]
type IStore =
    // Just get raw data from the store.
    // The necessary credentials will be passed via the "Live" object that 
    // actually implements the methods of the IStore interface
    // (as well as those of all other dependency interfaces).
    //abstract Get : GetRequestData -> Async<Result<string,DomainError>>
    // Alternative
    abstract GetSingleProduct : ProductId -> QueryParams option -> Async<Result<string,DomainError>>

// ===================================
// Helper functions
// ===================================

module Log = 

    let message (env: ILogger) fmt = 
        Printf.kprintf env.Message fmt

    module Dependency = 
        let message message = 
            Console.WriteLine $"MESSAGE: %s{message}"


module Store = 
    let getSingleProduct (env: #IStore) productId queryParams = 
        env.GetSingleProduct productId queryParams

// ===================================
// Implementation
// ===================================

module Api = 
    // FIXME: Make the parameter Response.headers,
    // get the content type inside the active pattern.
    let (|ResponseJson|ResponseOther|) (input: string) = 
        if input |> String.contains "json" then ResponseJson
        else ResponseOther


    let basicAuth creds =
        $"{creds.Key}:{creds.Secret}"
        |> Seq.map byte
        |> Array.ofSeq
        |> Convert.ToBase64String
        |> sprintf "Basic %s"

    // https://fsharp.slack.com/archives/C10DLSHQV/p1603059320303200
    let tryGetResponseAsync =
        HttpFs.Client.tryGetResponse
        >> Hopac.Alt.afterFun Choice.toResult
        >> Hopac.Job.toAsync
        >> AsyncResult.mapError (fun ex -> DomainError.Web ex.Message)


    let addQueryParams queryParamsOption (request: Request) = 
        let queryParams = 
            queryParamsOption
            |> Option.defaultValue Seq.empty

        queryParams
        |> Seq.fold (fun req (key, value) -> req |> Request.queryStringItem key value) request


    // Test: Wrong domain
    // Test: Wrong endpoint
    // Test: Wrong id
    let getRequest creds endpoint queryParams =
        let basicAuthValue = basicAuth creds
        let finalUrl = $"{creds.Url}/%s{endpoint}" 

        printfn $"Final URL: {finalUrl}"

        Request.createUrl Get finalUrl
        |> Request.setHeader (Custom ("Authorization", basicAuthValue))
        |> addQueryParams queryParams


    // FIXME: 
    let getProduct creds (productId: ProductId) queryParams = asyncResult {

        let productEndpoint = $"products/{productId}" 
        let productRequest = getRequest creds productEndpoint queryParams

        use! response = 
            productRequest
            |> tryGetResponseAsync


        let! getBodyAsync = 
            response
            |> Response.readBodyAsString
            |> Job.toAsync

        let errorMessageDecoder : Decoder<string> = 
            Decode.at [ "message" ] Decode.string 

        let errorMessageFromJson json = 
            Decode.fromString errorMessageDecoder json
            |> Result.mapError DomainError.Web

        let contentType = response.headers.[ContentTypeResponse]
        printfn $"Content-Type: {contentType}"

        match response.statusCode, contentType with
        | code, _ when code < 300 && code >= 200 -> 
            return getBodyAsync

        | _, ResponseJson  -> 
            match errorMessageFromJson getBodyAsync with
            | Error err -> return! Error err
            | Ok msg -> 
                let extendedMsg = $"Decoding error message for product ID {productId}:{msg}"
            
                return! Error (DomainError.Web extendedMsg)
        
        // Error with HTML response: 404 error, most probably because we messed up the address
        | _, ResponseOther -> 
            return! Error (DomainError.Web $"404: Address %s{creds.Url} not found")
    }

    

// Dependency implementation
// ===================================
let testStoreCredentials = {
    Key = "ck_808e7e7592b0ca7ae05056af1634b865ea12038a"
    Secret = "cs_64d1a37a8f28a72dd23383626a376a2222678827"
    Url = "https://woocommerce-130476-456889.cloudwaysapps.com/wp-json/wc/v3"
    // Url = "https://woocommerce-130476-456889.cloudwaysapps.com" // Test for "Page not found"
}


type TestDependencies (creds: Creds) = 

    interface ILogger with
        member _.Message message = 
            Log.Dependency.message message

    interface IStore with 
        member _.GetSingleProduct productId queryParams = 
            Api.getProduct creds productId queryParams

            
let testDependencies = TestDependencies testStoreCredentials


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

module OrderId = 

    let create id = { OrderId.Value = id }


module RawLineItem = 

    let jsonDecoder : Decoder<RawLineItem list> = 
        // https://fsharp.slack.com/archives/C10DLSHQV/p1631006489052100 
        let productIdDecoder = 
            Decode.int
            |> Decode.andThen (fun id -> 
                let productId = ProductId.create id

                match productId with
                | Ok value -> Decode.succeed value
                | Error msg -> Decode.fail msg
            )

        let decodeSingleItem = Decode.object (fun get -> {
            Id = get.Required.Field "id" productIdDecoder
            Name = get.Required.Field "name" Decode.string
        })
    
        Decode.at [ "line_items" ] (Decode.list decodeSingleItem)


    let fromJson json = 
        Decode.fromString jsonDecoder json
        |> Result.mapError DomainError.Decode
        

    // TODO: Add a function that takes a list of products and 
    let checkShippingRequired env (productId: ProductId)= 

        let idString = productId.ToString()

        asyncResult {
            let! productJson = Store.getSingleProduct env productId None

            Log.message env "Product Body: "

            let jsonDecoder = 
                Decode.object (fun get -> 
                {|
                    IsRequired = get.Required.Field "shipping_required" Decode.bool
                |})

            let! shipping = 
                Decode.fromString jsonDecoder productJson
                |> Result.mapError DomainError.Decode

            return shipping.IsRequired

        }


module RawProduct = 
    
    let fromRest = 
        //fun checkShippingRequired (lineItem: RawLineItem) ->
        fun env (lineItem: RawLineItem) ->

            asyncResult {
                let! shippingRequired = RawLineItem.checkShippingRequired env lineItem.Id

                let rawOrder = {
                    Data = lineItem
                    ShippingRequired = shippingRequired
                }

                Log.message env "New RawProduct from REST"
                return rawOrder
            }


module RawOrder = 

    let jsonDecoder (rawProducts: RawProduct list) : Decoder<RawOrder> = 
        
        Decode.object (fun get -> 
            let orderId = get.Required.Field "id" Decode.int
            {
                OrderId = OrderId.create orderId
                LineItems = rawProducts
            }
        )


    let fromJson json (rawProducts: RawProduct list) = 
        Decode.fromString (jsonDecoder rawProducts) json
        |> Result.mapError DomainError.Decode


// ===================================
// Let's test it
// ===================================

// This data comes from a webhook whenever an order is placed.
let dummyOrderJson = """
{
    "id": 1686,
    "line_items": [
    {
      "id": 1465,
      "name": "2-Years Warranty",
    },
    {
      "id": 8,
      "name": "Sunglasses",
    },
    {
      "id": 1321,
      "name": "Hard Case",
    },
    {
      "id": 1320,
      "name": "Cleaning Cloth",
    }
  ]
}
"""

// let dummyCreds = { Key = "myKey"; Secret = "mySecret" }

// let checkShippingRequired = 
//    RawLineItem.checkShippingRequired dummyCreds
    

// Helper function
let toList x = [ x ]

// Async<Result<list<RawProduct>,list<DomainError>>>
let getRawProducts env json = 
    asyncResult {
        // list<RawLineItem>
        let! rawLineItems = 
            RawLineItem.fromJson json
            |> Result.mapError toList // New: Result.mapError
        
        // Async<Result<list<RawProduct>,list<DomainError>>>
        let rawProducts = 
            rawLineItems
            |> List.map (RawProduct.fromRest env)
            |> List.sequenceAsyncResultA

        return! rawProducts
    }


// Async<Result<list<RawProduct>,list<DomainError>>>
// This is the same function as above, 
// but without using the AsyncResult computation expression.
let getRawProducts2 checkShippingRequired json = 
    // Result<list<RawLineItem>,DomainError>
    let rawLineItems = 
        RawLineItem.fromJson json
    
    // Async<Result<list<RawProduct>,list<DomainError>>>
    let getRawProductsAsyncResult rawItems = 
        rawItems
        |> List.map (fun rawItem -> 
            RawProduct.fromRest checkShippingRequired rawItem)
        |> List.sequenceAsyncResultA

    // Async<Result<list<RawProduct>,list<DomainError>>>
    let rawProducts = 
        rawLineItems
        |> AsyncResult.ofResult                       // New: AsyncResult.ofResult
        |> AsyncResult.mapError (fun e -> [ e ])      // New: AsyncResult.mapError
        |> AsyncResult.bind getRawProductsAsyncResult // New: AsyncResult.bind
    
    rawProducts


let rawOrder() = 
    dummyOrderJson
    |> getRawProducts testDependencies
    |> AsyncResult.bind (RawOrder.fromJson dummyOrderJson 
        >> AsyncResult.ofResult
        >> AsyncResult.mapError toList) // New: AsyncResult.mapError
    |> Async.RunSynchronously


// "Given an invalid JSON: Unexpected character encountered while parsing value: <. Path '', line 0, position 0."
           
 (*
    Ok
    { OrderId = { Value = 1686 }
    LineItems =
        [{ Data = { Id = { Value = 1 }
                    Name = "Product 1 " }       
                    ShippingRequired = true }; 
           
         { Data = { Id = { Value = 2 }
                    Name = "Product 2" }
                    ShippingRequired = true };

         { Data = { Id = { Value = 3 }
                    Name = "Product 3" }        
                    ShippingRequired = true }; 
           
         { Data = { Id = { Value = 4 }
                    Name = "Product 4" }
                    ShippingRequired = true }] }
*)
