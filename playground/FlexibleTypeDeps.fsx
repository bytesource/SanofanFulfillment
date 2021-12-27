#r "nuget: Microsoft.Data.Sqlite"

open System
open Microsoft.Data.Sqlite

// Dependency management with the help of flexible types.
// Code based on this blog post: 
// https://bartoszsypytkowski.com/dealing-with-complex-dependency-injection-in-f/#managing-dependencies-beyond-partial-application


// FIXME: How to turn organize the below code according to 
//        the onion architecture?

// ============================================
// Defining 2 sets of dependency interfaces
// ============================================

// Set 1: Logging
[<Interface>]
type ILogDeps = 
    abstract Debug : string -> unit
    abstract Error : string -> unit

[<Interface>]
type ILog = 
    abstract Logger : ILogDeps


// ============================================
// Set 2: Database

[<Interface>]
type IDbDeps = 
    abstract Query : string -> string


type IDb = 
    abstract Database : IDbDeps


// ============================================
// Functions using a single dependency via
// the 'env' parameter.
// ============================================


module Log = 
    let debug (env: #ILog) fmt = 
        Printf.kprintf env.Logger.Debug fmt

    let debugDep message = 
        Console.WriteLine ("ERROR: " + message)

// NOTE: Implement dependency?
module Db = 
    let fetchUser (env: #IDb) userId = 
        let user = env.Database.Query userId
        user


// ============================================
// Function using BOTH dependencies via the same
// 'env' parameter.
// ============================================

// env : 'a (requires :> IDb and :> ILog )
let fetchUserPlusLogging env userId = 
    let user = Db.fetchUser env userId
    Log.debug env $"Fetched user '%s{user}'"
    user



// ============================================
// Actually implementing the interfaces
// USING A COMBINING OBJECT
// ============================================

type Credentials = {
    Key: string
    Secret: string
}


// FIXME: Where and how do I pass the connection?
let newConnection () = new SqliteConnection("Data Source=hello.db")

type Live(creds: Credentials) = 
    interface ILog with
        member _.Logger = 
            { new ILogDeps with 
                member _.Debug message = 
                    Log.debugDep message // NOTE: Upstream module implementation.

                member _.Error message = 
                    Console.WriteLine ("ERROR: " + message)

            }
            
    interface IDb with
        member _.Database = 
            { new IDbDeps with 
                // Do some stuff with the database
                member _.Query userId =
                    let key = creds.Key  // Passing credentials!
                    "user"
            }


let liveCreds = {
    Key = "key"
    Secret = "secret"
}

let live = Live liveCreds


let getUser() = fetchUserPlusLogging live "12"
// ERROR: Fetched user 'user'
// val it : string = "user"