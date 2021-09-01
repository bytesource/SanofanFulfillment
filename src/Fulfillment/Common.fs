[<AutoOpen>]
module SanofanFulfillment.Common

open Credentials
open FSharp.Control.Tasks

open System

open System.IO
open System.Net
open System.Net.Mail
open Microsoft.AspNetCore.Http
open System.Threading.Tasks
open Falco

type DeveloperMode = bool
type ContentRoot = string
type PostsDirectory = string

let tryGetEnv (name : string) = 
    match Environment.GetEnvironmentVariable name with 
    | null 
    | ""    -> None 
    | value -> Some value


// =======================================
// My additions
// =======================================
module Option = 

    let ofString (str: string) = 
        if String.IsNullOrWhiteSpace str then
            None
        else Some str


let saveStringPlayground filename str = 
    // https://docs.microsoft.com/en-us/dotnet/api/system.io.path.combine
    // Path.Combine does not seems to add a trailing separator:
    // From the docs: d:\archives\2001\media\images

    // Use Path.DirectorySeparatorChar, NOT Path.PathSeparatorChar,
    // as it returned a semicolon (;)
    // let playgroundDir = Path.Combine(Directory.GetCurrentDirectory(), "Playground")
    let path = Path.Combine(Directory.GetCurrentDirectory(), "Playground", filename)

    File.WriteAllText(path, str)

module Request = 
    let getBodyString (ctx: HttpContext) = 
       task {
           let stream = ctx.Request.Body
           // https://www.codingvila.com/2021/05/reading-request-body-in-aspnet-mvc.html
           // stream.Seek(0L, System.IO.SeekOrigin.Begin) |> ignore
           // stream.Position <- 0L
           use reader = new StreamReader(stream)
           let! text = reader.ReadToEndAsync()
// TODO : Read this: https://devblogs.microsoft.com/aspnet/re-reading-asp-net-core-request-bodies-with-enablebuffering/
           return text
       }
       |> Async.AwaitTask
       |> Async.RunSynchronously

    
    let tryGetHeader headerKey ctx = 
        // Notice the lowercase "c" in "Wc". This is not in the documentation.
        let headerKey = "X-Wc-Webhook-Signature"
        let headers = ctx |> Request.getHeaders

        headers.TryGetString headerKey


[<RequireQualifiedAccess>]
module Email = 
    let configure smtp subject body  = 
        // https://www.c-sharpcorner.com/UploadFile/2a6dc5/how-to-send-a-email-using-Asp-Net-C-Sharp/

        let destination = smtp.To
        let from = smtp.From
        let message = new MailMessage(from, destination) 
         
        message.Subject <- subject
        message.Body <- sprintf $"%s{body}\ncode1234"
        message.BodyEncoding <- System.Text.Encoding.UTF8
        message.IsBodyHtml <- true

        // https://www.zoho.com/mail/help/zoho-smtp.html#alink3
        // Use TSL port
        // Use app password
        // I did NOT need to activate IMAP Access in the admin.
        let client = new SmtpClient(smtp.Server, smtp.Port)
        let credentials = NetworkCredential(smtp.To, smtp.Secret) // App password
        client.EnableSsl <- true
        client.UseDefaultCredentials <- false
        // Put after UseDefaultCredentials
        client.Credentials <- credentials
        
        client, message


    let sendAsync subject body = 
        let client, message = configure sanofanSmtp subject body
        client.SendMailAsync(message) |> Async.AwaitTask
         

module Response = 
    let toEmail subject body ctx = 
        Email.sendAsync subject body
        |> Async.RunSynchronously

        Response.ofEmpty ctx



open System.Text
open System.Security.Cryptography
module HvacValidation = 
// https://stackoverflow.com/questions/61865332/woocommerce-webhook-c-sharp-compare-hash

    let computHash (secret: string) (payload: string) = 
        // Set the encoding
        let encoding = Encoding.UTF8
        
        // Prepare the secret
        let secretBytes = encoding.GetBytes(secret)
        use hmac = new HMACSHA256(secretBytes)
        
        // Prepare the payload, aka the request body
        let payloadBytes = encoding.GetBytes(payload)

        // Compute the hash
        let hashBytes = hmac.ComputeHash(payloadBytes)
       
        // Convert hash byte array to string of base 64
        System.Convert.ToBase64String(hashBytes)






    



