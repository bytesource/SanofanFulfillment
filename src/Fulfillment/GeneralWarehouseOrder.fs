module SanofanFulfillment.GeneralWarehouseOrder

module Common = 

    // All countries we have shipment routes set up for.
    type CountryCode = 
        | US
        | UK
        // AU, NZ

    
    [<RequireQualifiedAccess>]
    module CountryCode = 
        
        /// Might fail because we don't have all countries implemented yet
        let validate(codeString: string) = 
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


open RawOrder
open Common

// Order
// ===========================
type OrderId = private OrderId of int
with 
    static member op_Explicit(OrderId id) = id

module OrderId = 

    let create id = OrderId id


/// Selected order status we are dealing with 
/// when processing an active order.
/// Hence, this is not a list of all available order status.
type OrderStatus = 
    | Processing
    | PendingShipment

module OrderStatus = 

    let validate orderId input = 
        // Currently only using built-in and custom order status
        // from WooCommerce.
        match input with 
        | 





// Address
// ===========================
type Postcode = private Postcode of string

module Postcode = 
    // We'll validate at the specific warehouse step
    let create postcode = 
        Postcode postcode


// Payment
// ===========================
type TransactionId = private TransactionId of string

module TransactionId = 

    let create id = TransactionId  id


type PaymentMethod = 
    | PayPal of TransactionId
    | Stripe of TransactionId
    | COD

module PaymentMethod = 

    let validate (oId: OrderId) tId paymentMethod = 
        match paymentMethod with 
        | "ppec_paypal" -> Ok (PayPal tId)
        | "stripe"      -> Ok (Stripe tId)
        // Cash on delivery. Used during testing
        | "cod"         -> Ok COD
        | _ -> 
            let msg = $"Order# %i{int oId}: Payment method '{paymentMethod}' not recognized."
            Error [ msg ]


module String = 

    let capitalizeWords (str: string) = 
        let words = System.Text.RegularExpressions.Regex.Split(str, @"\s+")

        words
        |> Seq.map (fun word -> 
           let firstUpper = word.[0].ToString().ToUpper()
           let restLower  = word.[1..].ToLower()
           firstUpper + restLower)
        |> String.concat " "


// Note: I'm not validating the email at this point because:
// -- I'm using an email validation service that checks,
//    if an email address actually exists.
// -- The regex won't catch the most common typos like "...gmail.co"
// -- The email address field is optional for most carriers.
// NOTE: We should still validate, because the email might 
//       come from somewhere else.
type Email = private Email of string
with 
    override this.ToString() = 
        let (Email email) = this
        email


// TODO:
// Capitalize words
// Remove not allowed characters (see USPS list)
// Remove umlauts??

        
type GeneralWarehouseOrder = {

    OrderId: OrderId

    
    Payment: PaymentMethod
    ShipTo: RawAddress

}