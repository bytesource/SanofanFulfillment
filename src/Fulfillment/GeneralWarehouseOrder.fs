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
        /// Also, country names might change over time, 
        /// so we should always return a Result.
        /// https://github.com/woocommerce/woocommerce/blob/trunk/i18n/countries.php
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
        | "processing" -> Ok Processing
        | "pending-shipment" -> Ok PendingShipment
        | _ -> 
            let msg = $"Order {int orderId}: Unknown status '{input}'"
            Error [ msg ]


type OrderTotal = private OrderTotal of float
with
    static member op_Explicit(OrderTotal value) = float value

module OrderTotal = 

    let create input = OrderTotal input


// Address
// ===========================
type Postcode = private Postcode of string

module Postcode = 
    // We'll validate at the specific warehouse step
    let create postcode = 
        Postcode postcode


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
TODO: 
// Continue here. Implement this PHP method, as I'll need it anyway.


// Payment
// ===========================
type PaymentId = private PaymentId of string

module TransactionId = 

    let create id = PaymentId  id


type PaymentMethod = 
    | PayPal of PaymentId
    | Stripe of PaymentId
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


// Payment
// ===========================

type SKU = private SKU of string
with 
    override this.ToString() = 
        let (SKU skuString) = this
        skuString

module SKU = 
    // validate if Result
    let create skuString = 
        SKU skuString


type ProductId = private { Value: int }
with 
    static member op_Explicit(productId) = 
        productId.Value

module ProductId = 

    let create id = { Value = id }


type ProductPrice = private { Value: float }
with 
    static member op_Explicit(price) = price.Value

module ProductPrice = 

    let create input = { Value = input }


type Product = {
    Name: string
    Id: ProductId
    SKU: SKU
    Price: ProductPrice
}

type WarehouseLineItem = 
    | Shippable of Product
    | NonShippable of Product



// Helpers
// ===========================

module String = 

    let capitalizeWords (str: string) = 
        let words = System.Text.RegularExpressions.Regex.Split(str, @"\s+")

        words
        |> Seq.map (fun word -> 
           let firstUpper = word.[0].ToString().ToUpper()
           let restLower  = word.[1..].ToLower()
           firstUpper + restLower)
        |> String.concat " "



        
type GeneralWarehouseOrder = {

    OrderId: OrderId
    Status: OrderStatus
    Total: OrderTotal

    LineItems: WarehouseLineItem list
    
    Payment: PaymentMethod
    ShipTo: RawAddress

}