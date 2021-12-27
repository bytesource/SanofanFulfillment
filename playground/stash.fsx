open Credentials
open FSharp.Control.Tasks

open System

open System.IO
open System.Net
open System.Net.Mail
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Falco


let sendBodywithEmailAsync (subject: string) (ctx: HttpContext) = 
    let stream = ctx.Request.Body
    let reader = new StreamReader(stream)

    async {

        let! text = reader.ReadToEndAsync() |> Async.AwaitTask

        return! sendEmailAsync subject text
    }


// Code by @zaid_ajaj
// https://fsharp.slack.com/archives/C10DLSHQV/p1627981859060700
let sendBodywithEmail subject = 
    fun (ctx: HttpContext) -> 
        task {
            let stream = ctx.Request.Body
            use reader = new StreamReader(stream)
            let! text = reader.ReadToEndAsync()
            do! Async.StartAsTask(sendEmailAsync subject text)
            return! Response.ofEmpty ctx
        }


let headers (ctx: HttpContext) = 
    ctx.Request.Headers.Keys
    |> String.concat ","


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
    