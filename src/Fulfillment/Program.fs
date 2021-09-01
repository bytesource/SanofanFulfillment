module SanofanFulfillment.Program

open Domain
open Credentials

open Falco
open Falco.Routing
open Falco.HostBuilder
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection


// ------------
// Register services
// ------------
let configureServices (services : IServiceCollection) =
    services.AddFalco() |> ignore

// ------------
// Activate middleware
// ------------
let configureApp (endpoints : HttpEndpoint list) (ctx : WebHostBuilderContext) (app : IApplicationBuilder) =    
    let devMode = StringUtils.strEquals ctx.HostingEnvironment.EnvironmentName "Development"    
    app.UseWhen(devMode, fun app -> 
            app.UseDeveloperExceptionPage())
       .UseWhen(not(devMode), fun app -> 
            app.UseFalcoExceptionHandler(Response.withStatusCode 500 >> Response.ofPlainText "Server error"))
       .UseFalco(endpoints) |> ignore

// -----------
// Configure Web host
// -----------
let configureWebHost (endpoints : HttpEndpoint list) (webHost : IWebHostBuilder) =
    webHost
        .ConfigureServices(configureServices)
        .Configure(configureApp endpoints)


[<EntryPoint>]
let main args =   
    webHost args {
        configure configureWebHost


        endpoints [            
            get "/" (Response.ofPlainText "Hello world")

            post "/new-order" (fun ctx -> 
                let payload = ctx |> Request.getBodyString

                let headerValue = 
                    Request.tryGetHeader wooSignatureHeaderKey ctx
                    |> function 
                        | Some signature -> 
                            let hash = HvacValidation.computHash wooSecret payload
                            if hash = signature then 
                                "Signature matched!" 
                            else $"Signature: {signature}, Hash: {hash}"
                        | None -> sprintf "No signature header found"


                let jsonToString payload = 
                    payload
                    // NOTE: Using RawOrderItem during testing!
                    |> RawOrderItem.FromJson
                    |> sprintf "From JSON: %A"

                Response.toEmail "Testing Thoth" (jsonToString payload) ctx
                // responseWithEmail headerValue ctx) 
                // sendBodywithEmail headerValue ctx) 
            )

            get "/new-order" (fun ctx -> 
                let headerValue = 
                    Request.tryGetHeader wooSignatureHeaderKey ctx
                    |> function 
                        | Some value -> value
                        | None -> sprintf "No header value found"

                let headers = 
                    ctx.Request.Headers.Keys
                    |> String.concat ","


                Response.toEmail headers "Dummy Body" ctx) 
        ]
    }
    0