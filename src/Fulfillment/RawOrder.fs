module SanofanFulfillment.RawOrder

open System.IO
open Thoth.Json.Net
open Credentials

open FsToolkit.ErrorHandling



// TODO: 
// 5) Consider separating the decoder code into another file, e.g. Order.Validation

let getOrderJsonString = 
    let file = "WooWebhookNewOrderResponseBody.json"
    let filePath = Path.Combine(Directory.GetCurrentDirectory(), "Playground", file) 
    printfn $"getOrderJson file path: {filePath}"
    File.ReadAllText filePath

let getProductsJsonString = 
    let file = "WooAllProducts.json"
    let filePath = Path.Combine(Directory.GetCurrentDirectory(), "Playground", file) 
    printfn $"getOrderJson file path: {filePath}"
    File.ReadAllText filePath

// ====================================================
// Extract order data from the WooCommerce 
// 'Order.received' webhook
// ====================================================

// NOTE: WooCommerce removes all leading and trailing whitespace.
type RawPayment = {
    PaymentMethod: string
    TransactionId: string
}
with 
    static member JsonDecoder : Decoder<RawPayment> = 
        Decode.object(fun get -> {
            PaymentMethod = get.Required.Field "payment_method" Decode.string
            TransactionId = get.Required.Field "transaction_id" Decode.string
        })


type LineItemData = {
    ItemId: int
    Sku: string
    Name: string
    Price: float
}
with 
    static member JsonDecoder : Decoder<LineItemData list> = 
        let decodeOrderItem : Decoder<LineItemData> =
            Decode.object (fun get -> {
                ItemId = get.Required.Field "id" Decode.int
                Sku = get.Required.Field "sku" Decode.string
                Name = get.Required.Field "name" Decode.string
                Price = get.Required.Field "total" (Decode.map float Decode.string)
            })
        
        Decode.at ["line_items"] (Decode.list decodeOrderItem)
       

    static member FromJson json = 
        Decode.fromString LineItemData.JsonDecoder json


type RestProduct = {
    Id: int
    Sku: string // Not always present
    Shippable: bool
}
with 
    static member JsonDecoder : Decoder<RestProduct> = 
        let decodeProduct : Decoder<RestProduct> =
            Decode.object (fun get -> {
                Id = get.Required.Field "id" Decode.int
                Sku = get.Required.Field "sku" Decode.string
                Shippable = get.Required.Field "shipping_required" Decode.bool
            })

        decodeProduct

    static member FromJson json = 
        Decode.fromString RestProduct.JsonDecoder json




// ===========================================
// Test tube START
// ===========================================
// TODO:
// Pack Woo credentials in record
// Can I "bake-in credentials?"
// Bake the redentials in!!!!
// type GetRestProduct = 
//     Credentials.REST -> int -> Async<Result<RestProduct, string list>>

type GetRestProduct = 
    int -> Async<Result<RestProduct, string list>>

// Dummy implementation
let getWooRestProduct (creds: Credentials.REST) : GetRestProduct = 
    fun productId ->
       async {
        return Ok {
            Id = productId
            Sku = "123"
            Shippable = true
           }
       }


type SortedLineItem = 
    | Shippable of LineItemData
    | NonShippable of LineItemData


module SortedLineItem = 

    // Dummy implementation
    let create : GetSortedLineItem =
        fun getproduct lineItemData ->
            async {
                return Ok (Shippable lineItemData)
            }


    let create



let getProduct = getWooRestProduct Credentials.Testing.wooRest


let lineItemDataColl = 
    LineItemData.FromJson getOrderJsonString
    |> Result.mapError (fun e -> [ e ])


let sortedLineItems = 
    lineItemDataColl
    |> Result.map (fun itemList -> 
        itemList 
        |> List.map (fun item -> 

            item |> SortedLineItem.create getProduct
        ))
    |> Result.map (fun asyncResultSortedItems -> 
        let result = 
            asyncResultSortedItems
            |> Async.Parallel
            |> Async.RunSynchronously

        result)
    |> Result.map Array.toList



module Async = 
    
    let map (f: 'a -> 'b) (aVal: Async<'a>) : Async<'b> = 
        async {
            let! result = aVal

            return (f result)
        }

    // RestProduct -> LineItemData -> SortedLineItem

    // ((RestProduct -> SortedLineItem) -> C<RestProduct,_> -> C<SortedLineItem,_>)

type GetRestProduct2 = 
    int -> Async<Result<RestProduct, string list>>

type GetSortedLineItem = LineItemData -> RestProduct -> SortedLineItem
// Go fromAsync<Result<RestProduct>> to a Async<Result<SortedLineItem>>

let sortedLineItems // Result<list<Async<Result<SortedLineItem list...
    (getProduct: GetRestProduct2)
    (getSortedLineItem: GetSortedLineItem) =

    lineItemDataColl // Result<LineItemData list>
    |> Result.map (List.map (fun item ->

        asyncResult {
            let! restProduct = getProduct item.ItemId
            let sortedLineItem = getSortedLineItem item restProduct

            return sortedLineItem
        } ))



type RawAddress = {
    FirstName: string
    LastName: string
    Email: string
    Phone: string option

    AddressLine1: string
    // No need to use Option here as we'll combine both address lines 
    // into a single string later anyway. 
    AddressLine2: string

    City: string
    // All carriers in our main markets require the state
    // with international shipments.
    // BUT on Woo the state for the UK is optional.
    State: string option
    Postcode: string
    CountryCode: string

}
with
    static member JsonDecoder : Decoder<RawAddress>  = 
        let addressDecoder = 
            Decode.object (fun get -> {
                FirstName = get.Required.Field "first_name" Decode.string
                LastName = get.Required.Field "last_name" Decode.string
                Email = get.Required.Field "email" Decode.string
                Phone = get.Required.Field "phone" Decode.string |> Option.ofString

                AddressLine1 = get.Required.Field "address_1" Decode.string
                AddressLine2 = get.Required.Field "address_2" Decode.string

                City = get.Required.Field "city" Decode.string
                State = get.Optional.Field "state" Decode.string
                CountryCode = get.Required.Field "country" Decode.string
                Postcode = get.Required.Field "postcode" Decode.string
            })
        
        // NOTE: The way the shop is set up, we need to get the shipping data
        //       from the billing field.
        Decode.at ["billing"] addressDecoder


[<RequireQualifiedAccess>]
type RawOrder = {
    OrderId: int
    Status: string
    Total: float

    Payment: RawPayment

    LineItems: SortedLineItem list

    ShipTo: RawAddress
}
with 
    // TODO: need to add GetRestProduct dependency as well.
    //Or just use the GetRestProduct dependency
    static member JsonDecoder 
        (sortedLineItems:  SortedLineItem list)
        : Decoder<RawOrder> = 

        Decode.object(fun get -> {
            OrderId = get.Required.Field "id" Decode.int 
            Status = get.Required.Field "status" Decode.string 
            Total = get.Required.Field "total" (Decode.map float Decode.string)

            Payment = get.Required.At [] RawPayment.JsonDecoder

            // Returns Async<Result<SortedLineItem>
            LineItems = sortedLineItems

            ShipTo = get.Required.At [] RawAddress.JsonDecoder
        })

    static member FromJson (sortedLineItems: SortedLineItem list) json = 
        Decode.fromString (RawOrder.JsonDecoder sortedLineItems) json

