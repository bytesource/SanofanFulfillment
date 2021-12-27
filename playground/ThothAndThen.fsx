#r "nuget: Thoth.Json.Net"
#r "nuget: FsToolkit.ErrorHandling"

open System
open System.IO
open Thoth.Json.Net
open FsToolkit.ErrorHandling


// ======================================
// Helper functions
// ======================================
module Result = 
    
    // applyA and sequenceA adapted from "Domain Modeling Made Functional"
    let applyA fVal xVal = 
        match fVal, xVal with 
        | Ok f, Ok x -> Ok (f x)
        | Error e1, Ok _ -> Error e1  // e1 has to be a list
        | Ok _, Error e2 -> Error e2  // e2 has to be a list
        | Error e1, Error e2 -> Error (e1 @ e2)


    let sequenceA resultList = 
           let ( <!> ) = Result.map
           let ( <*> ) = applyA // Only difference to sequenceM

           let cons head tail = head :: tail
           let consR headR tailR = cons <!> headR <*> tailR
           let initVal = Ok []

           // Loop through the list, prepending each element
           // to the initial value
           List.foldBack consR resultList initVal


module Async = 
    let map f asyncVal = 
        async {
            let! value = asyncVal
            return (f value)
        }


    let bind f asyncVal = 
        async {
            let! value = asyncVal
            return! (f value)
        }


// ==================================
// Domain Types
// ==================================

// ===================================
// Implementation

type TestId = TestId of int

module TestId = 
    let create id : Result<TestId, string> = 
        if id > 0 then Ok (TestId id) else Error "Test ID must be positive"

type SomeRecord = {
    Id: TestId
    More: string
}

let jsonDecoder : Decoder<SomeRecord> = 
    
    let testIdDecoder = 
        Decode.int
        |> Decode.andThen (fun id ->
            let testId = TestId.create id

            match testId with
            | Ok id -> Decode.succeed id
            | Error msg -> Decode.fail msg
        )

    Decode.object (fun get -> {

        Id = get.Required.Field "id" testIdDecoder
        More = get.Required.Field "more" Decode.string
    })