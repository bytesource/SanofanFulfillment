#r "nuget: Thoth.Json.Net"
#r "nuget: FsToolkit.ErrorHandling"

open System
open System.IO
open Thoth.Json.Net
open FsToolkit.ErrorHandling


// ======================================
// Helper functions
// ======================================


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
}


type ProductId = private { Value: int } with
    static member op_Explicit(productId) = productId.Value

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


type CheckShippingRequired = ProductId -> Async<Result<bool,DomainError>>


// type CreateRawProduct = 
//     CheckShippingRequired // Dependency
//       -> RawLineItem          // Input
//       -> Async<Result<RawProduct,DomainError>>  // Output


type RawOrder = {
    OrderId: OrderId
    LineItems: RawProduct list
}



// ===================================
// Implementation
// ===================================


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
        


    // NOTE: Dummy implementation
    let checkShippingRequired = 
        fun creds lineItem ->
            // Dummy REST call response
            async { return Ok true }


module RawProduct = 
    
    let fromRest = 
        //fun checkShippingRequired (lineItem: RawLineItem) ->
        fun (checkShippingRequired: ProductId -> Async<Result<bool,DomainError>>) (lineItem: RawLineItem) ->

            asyncResult {
                // NOTE: Compiler error (without type annotation for 'checkShippingRequired'):
                // A unique overload for method 'Source' could not be determined
                // based on type information prior to this program point. 
                // A type annotation may be needed.
                let! shippingRequired = checkShippingRequired lineItem.Id

                let rawOrder = {
                    Data = lineItem
                    ShippingRequired = shippingRequired
                }

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
      "id": 1,
      "name": "Product 1 ",
    },
    {
      "id": 2,
      "name": "Product 2",
    },
    {
      "id": 3,
      "name": "Product 3",
    },
    {
      "id": 4,
      "name": "Product 4",
    }
  ]
}
"""

let dummyCreds = { Key = "myKey"; Secret = "mySecret" }

let checkShippingRequired : CheckShippingRequired = 
    RawLineItem.checkShippingRequired dummyCreds
    

// Helper function
let toList x = [ x ]

// Async<Result<list<RawProduct>,list<DomainError>>>
let getRawProducts checkShippingRequired json = 
    asyncResult {
        // list<RawLineItem>
        let! rawLineItems = 
            RawLineItem.fromJson json
            |> Result.mapError toList // New: Result.mapError
        
        // Async<Result<list<RawProduct>,list<DomainError>>>
        let rawProducts = 
            rawLineItems
            |> List.map (RawProduct.fromRest checkShippingRequired)
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
    |> getRawProducts checkShippingRequired
    |> AsyncResult.bind (RawOrder.fromJson dummyOrderJson 
        >> AsyncResult.ofResult
        >> AsyncResult.mapError toList) // New: AsyncResult.mapError
    |> Async.RunSynchronously
           
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
