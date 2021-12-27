#r "nuget: Ply"
#r "nuget: Newtonsoft"

open System
open Ply

// https://bartoszsypytkowski.com/dealing-with-complex-dependency-injection-in-f/

[<Interface>]
type ILogger = 
    abstract Debug: string -> unit
    abstract Error: string -> unit

// Dependency 1
[<Interface>]
type ILogMe = 
    abstract Logger: ILogger

module LogMe = 
    let debug (env: #ILogMe) fmt = 
        Printf.kprintf env.Logger.Debug fmt

    let error (env: #ILogMe) fmt = 
        Printf.kprintf env.Logger.Error fmt


// =================================


// Dependency 2
[<Interface>]
type IDatabase = 
    abstract Query: string * 'input -> Task<'output>
    abstract Execute: string * 'input -> Task<unit>


[<Interface>] 
type IDb = 
    abstract Database: IDatabase


module Db = 
    let fetchUser (env: #IDb) userId = 
        env.Database.Query(Sql.FetchUser, {| userId = userId |})

    let updateUser (env: #IDb) user = 
        env.Database.Execute(Sql.UpdateUser, user)


// 1) NOTE: When using functions from different modules
//    that all take an 'env' parameter,
//    the F# compiler infers the parameter as a union
//    of all matching interfaces. 

// val env : :> IDb and :> ILog
let foo env = 
    let user = Db.fetchUser env 123 // env :> IDb
    LogMe.debug env "User: %A" user   // env :> ILog



// 2) Why use separate interfaces (ILog/ILogger)?
//    This approach let's us isolate the capabilities 
//    of different modules:


// =================================
// https://gfritz.github.io/posts/2020-12-05-fsadvent-2020-dependency-injection-using-flexible-types-and-type-inference.html

/// Application will log via these methods.
/// This is what the application logic needs.
[<Interface>]
type IAppLogger = 
    abstract Debug: string -> unit 
    abstract Error: string -> unit 


/// The 'env' object wil use this interface 
/// This is what the flexible types annotation uses
/// to enable to compiler to IMPLICITLY add the dependency
/// to the 'env' type definition.
[<Interface>]
type ILog = 
    abstract Logger: IAppLogger


/// #ILog means that 'env' can be ANY TYPE that is
/// COMPATITBLE with the ILog interface.
/// This is the 'flexible type' annotation that allows
/// the compiler to infer a compatible interface - it 
/// figures out the dependency for us.
module Log = 
    let debug (env: #ILog) fmt = 
        Printf.kprintf env.Logger.Debug fmt

    let error (env: #ILog) fmt = 
        Printf.kprintf env.Logger.Error fmt

// NOTE: ACTUAL IMPLEMENTATION of IAppLogger interface.
/// Adapt the dependency to IAppLogger.
/// (If the loggers needs configuration, you can add
/// any config object as a parameter to 'live'.)
let live : IAppLogger = 
    { new IAppLogger with
        member _.Debug message = 
            Console.WriteLine ("DEBUG: " + message)
        member _.Error message = 
            Console.WriteLine ("ERROR: " + message)}

/// Test it all out!
/// val findUser:
//   env       : ILog   ->
//   searchTerm: string -> unit
let findUser env = 
    fun searchTerm -> 
        Log.debug env $"Search for this: '%s{searchTerm}'"


// =======================================
// Using 2 interfaces as a dependency at once
// =======================================

// First, let's a another dependency
// NOTE: ISerializer not needed here. 
//       This example is about IStashUsers below.

[<Interface>]
type ISerializer = 
    abstract Deserialize<'t> : string -> Async<'t>
    abstract Serialize : 't -> string

module Serializer = 
    open Newtonsoft.Json
    open Newtonsoft.Json.Serialization

    let private settings = JsonSerializerSettings()
    settings.ContractResolver <- CamelCasePropertyNamesContractResolver()


    let live = 
        { new ISerializer with
            member _.Deserialize<'t> httpContent = 
                async {
                    return 2
                }
                
            member _.Serialize toSerialize = 
                "hello"
        }

[<Interface>]
type IStashUsers = 
    abstract GetUserByName: string -> Async<string>

[<Interface>]
type IStashApi = 
    abstract Users : IStashUsers


module StashUsers = 
    let getUserByName (env: #IStashApi) searchTerm = 
        env.Users.GetUserByName searchTerm


    let live 
        (serializer: ISerializer) 
        stashApiUrl 
        accessToken
        : IStashUsers 
        =

        { new IStashUsers with
            member _.GetUserByName userName = 
                async {
                    return "hello"
                }
        }



// Finally, using two dependencies together:

let findUser2Deps env = 
    fun searchTerm -> 

    Log.debug env $"Searching for: '%s{searchTerm}'"

    // Page response
    let x = StashUsers.getUserByName env searchTerm

    // option<UserDto>
    let user = "Thomas"

    Log.debug env $"Best match for {searchTerm} is {user} "

    user


