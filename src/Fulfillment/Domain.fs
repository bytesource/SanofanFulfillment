module SanofanFulfillment.Domain


// ======================================
// Validation of warehouse order
// First option
// ======================================

type AsciiAddress = private AsciiAddress of string
with
    member this.Value() = 
        let (AsciiAddress address) = this
        address


type SfcAddress = private SfcAddress of string

module AsciiAddress = 
    // NOTE: I probably don't need to return a Result, just convert to askii
    let create (entry: string) = 
        // Convert umlauts
        // Remove '-' and '_'
        // Convert to ascii
        AsciiAddress entry


module String = 

    let inRange min max str = 
        let strLength = str |> String.length

        strLength >= min && strLength <= max


    
module SfcAddress = 
    
    let create entry carrier = 
        let (AsciiAddress address) = AsciiAddress.create entry

        match carrier with
        | "DHL" ->
            if address |> String.inRange 3 30 then
                Ok (SfcAddress address)
            else 
                Error "DHL address out of range"
        | _ -> Ok (SfcAddress address)


// ======================================
// Validation of warehouse order
// Another option
// ======================================
type AsciiText = private AsciiText of string

module AsciiText = 
    let create (input: string) = 
        AsciiText input


module Warehouse = 
    [<RequireQualifiedAccess>]
    module SFC = 

        type Address = private Address of string

        module Address = 

            let create carrier (AsciiText address) = 

                match carrier with
                | "DHL" ->
                    if address |> String.inRange 3 30 then
                        Ok (Address address)
                    else 
                        Error ["DHL address out of range"]
                | _ -> Ok (Address address)


open Warehouse

// NOTE: Add here to see if there will be a name collision
type Address = Address of string

type SfcTest = {
    Name: AsciiText
    Address: SFC.Address
}


type Validation<'Success, 'Failure> = 
    Result<'Success, 'Failure list>


/// Functions for the Validation type
/// (mostly applicative)
[<RequireQualifiedAccess>]
module Result = 

    /// Alias for Result.map
    let map = Result.map

    /// Apply a Validation<fn> to a Validation<x>,
    /// applicatively
    // NOTE: Returns a Result<_,_> if the return value Validation is not added.
    // let applyA (fVal: Validation<_,_>) (xVal: Validation<_,_>) = 
    let applyA (xVal: Result<_,_>) (fVal: Result<_,_>) : Validation<_,_> = 
        match fVal, xVal with 
        | Ok f, Ok x -> Ok (f x)
        | Error e1, Ok _ -> Error e1  // e1 has to be a list
        | Ok _, Error e2 -> Error e2  // e2 has to be a list
        | Error e1, Error e2 -> Error (e1 @ e2)


let sfcRecord = 
    let record name address = 
        { Name =  name; Address = address }
    let ascii = AsciiText.create "My Address"
    let sfcAddress = SFC.Address.create "DHL" ascii

    Ok record
    |> Result.applyA (Ok ascii)
    |> Result.applyA sfcAddress

// NOTE: Type of the Address field is marked as 
//       'Address' and not 'SFC.Address'
//       This is confusing...
// NOTE: Maybe just work with type SfcAddress = ... instead of heavy nesting.
(*
    val sfcRecord : Result<SfcTest,string list> =
  Ok { Name = AsciiText "My Address"
       Address = Address "My Address" }
*)



