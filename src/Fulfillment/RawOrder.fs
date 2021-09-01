module SanofanFulfillment.RawOrder

open System.IO
open Thoth.Json.Net

// TODO: 
// 5) Consider separating the decoder code into another file, e.g. Order.Validation

let getOrderJsonString = 
    let file = "WoocommerceWebhookNewOrderResponseBody.json"
    let filePath = Path.Combine(Directory.GetCurrentDirectory(), "Playground", file) 
    printfn $"getOrderJson file path: {filePath}"
    File.ReadAllText filePath


// ====================================================
// Order data extracted from 'Order.received' webhook
// ====================================================

// NOTE: WooCommerce removes all leading and trailing whitespace.
// TODO: Raw -> Woo (all fields are specific to WooCommerce)
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


type RawLineItem = {
    ItemId: int
    Sku: string
    Name: string
    Price: float
}
with 
    static member JsonDecoder : Decoder<RawLineItem list> = 
        let decodeOrderItem : Decoder<RawLineItem> =
            Decode.object (fun get -> {
                ItemId = get.Required.Field "id" Decode.int
                Sku = get.Required.Field "sku" Decode.string
                Name = get.Required.Field "name" Decode.string
                Price = get.Required.Field "total" (Decode.map float Decode.string)
            })
        
        Decode.at ["line_items"] (Decode.list decodeOrderItem)
       

    static member FromJson json = 
        Decode.fromString RawLineItem.JsonDecoder json


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

    LineItems: RawLineItem list

    ShipTo: RawAddress
}
with 
    static member JsonDecoder : Decoder<RawOrder> = 
        Decode.object(fun get -> {
            OrderId = get.Required.Field "id" Decode.int 
            Status = get.Required.Field "status" Decode.string 
            Total = get.Required.Field "total" (Decode.map float Decode.string)

            Payment = get.Required.At [] RawPayment.JsonDecoder

            LineItems = get.Required.At [] RawLineItem.JsonDecoder

            ShipTo = get.Required.At [] RawAddress.JsonDecoder
        })

    static member FromJson json = 
        Decode.fromString RawOrder.JsonDecoder json

