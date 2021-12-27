#r "nuget: Thoth.Json.Net"
#r "nuget: FsToolkit.ErrorHandling"

open System
open System.IO
open Thoth.Json.Net
open FsToolkit.ErrorHandling



type DummyOrder = {
    OrderId: int
    LineItems: RawOrderItem list 
    ShipTo: RawAddress
}
with 
    static member JsonDecoder : Decoder<DummyOrder> = 
        Decode.object (fun get -> {
            OrderId = get.Required.Field "id" Decode.int
            LineItems = get.Required.At [] RawOrderItem.JsonDecoder
            ShipTo = get.Required.At [] RawAddress.JsonDecoder
        })

    static member FromJson json = 
        // TODO: When ready, change back to RawOrder.Decoder
        Decode.fromString DummyOrder.JsonDecoder json


let rawOrderItemsJson = 
    RawOrderItem.FromJson getOrderJsonString

let dummyOrderJson() = 
    DummyOrder.FromJson getOrderJsonString

let decodeRawOrder() = 
    RawOrder.FromJson getOrderJsonString


// TODO: Validation
// 1) How to validate? 
// -- Per warehouse company?
//    (in the US, I might use this Chinese company that has its own rules)
// -- Per route?
// 2) Where to put the validation?
// -- On the single type? Per Carrier? 
type private CarrierAddress = CarrierAddress of string
module CarrierAddress = 
    let create carrier addressString = 
        match carrier with
        | "SFC" -> Ok addressString
        | _ -> Error "Carrier not recognized"

type Carrier = 
    | DHL
    | RoyalMail

type SanTaiAddress = SanTaiAddress of string

module SanTaiAddress = 
    
    let validate (carrier: Carrier) (address: string) : Result<SanTaiAddress, string> =
        Ok (SanTaiAddress "hello")

type GetCarrier = RawOrder -> Carrier

type GetAddress = Carrier -> string -> Result<SanTaiAddress, string>

// FIXME: How to bring in the AsciiText that is used everywhere?
// IDEA: Add an intermediate type RawWarehouseOrder after RawOrder
//       that can do the following:
// -- Transform text to ASCII
// -- Verify email
// -- Country Code
// -- ... other verifications that 
//    -- most probably are the same accross different 
//       fulfillment providers AND THAT
//    -- Covers all "must-have" verifications
//
// usuable to uploade
let createSanTaiAddress 
    (getCarrier: GetCarrier) // Dependency
    (getAddress: GetAddress) // Dependency
    (rawOrder: RawOrder) = 

    let carrier = getCarrier rawOrder
    // FIXME: Combine both address lines
    let rawAddress = rawOrder.ShipTo.AddressLine1

    match carrier with
    | DHL -> 
        getAddress carrier rawAddress

    | _ -> Error "Carrier not recognized"

// TODO: We need a default for everything, and use a special validation for a field if 
//       required by the carrier.


// TODO for email:
// Send email if we automatically correct an entry, 
// e.g. changing the zip code from 12345-1234 to 12345
// TODO: Treat country code IM (Isle of man) as UK 

type Quality =
    private { Value' : byte }

    static member Of (number: int) = 
        { Value' = byte number }

    static member Of (number: string) = 
        { Value' = byte 3 }
    
    override me.ToString() = 
        $"%02i{me.Value'}"

    // NOTE other behavior elided

    static member op_Explicit(quality : Quality) : int = 
        int quality.Value'

let quality = Quality.Of(7)
printfn $"%s{string quality}, or %i{int quality}"
// 07, or 7


let quality2 = Quality.Of("dummy")
printfn $"%s{string quality2}, or %i{int quality2}"
// 03, or 3


module Common = 

    type CountryCode = 
        | US
        | UK

    
    [<RequireQualifiedAccess>]
    module CountryCode = 
        
        /// Might fail because we don't have all countries implemented yet
        let create(codeString: string) = 
            let codeToUpper = codeString.ToUpper()
            match codeToUpper with
            | "US" | "UNITED STATES"  -> Ok US
            | "UK" | "UNITED KINGDOM" -> Ok UK 
            | other -> 
                let msg = $"CountryCode.create: '{other}' not recognized as CountryCode"
                Error "msg"


        let toCodeString countryCode = 
            match countryCode with
            | US -> "US"
            | UK -> "UK"


        let toCountryNameString countryCode = 
            match countryCode with
            | US -> "UnitedStates"
            | UK -> "United Kingdom"

type SKU = private SKU of string
with 
    override this.ToString() = 
        let (SKU skuString) = this
        skuString

module SKU = 
    // validate if Result 
    let create skuString = 
        SKU skuString

type Product = {
    Name: string
    SKU: SKU
}

type ProductType = 
    | Shippable of Product
    | NonShippable of Product

// TODO: How to determine if a product is shippable or
//       non-shippable?


        


