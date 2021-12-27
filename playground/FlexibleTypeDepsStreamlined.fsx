open System

// ============================================
// Defining 2 sets of dependency interfaces
// ============================================

// IDEA: Define these as I need them in the code

// Set 1: Logging
[<Interface>]
type ILogger = 
    abstract Debug : string -> unit
    abstract Error : string -> unit


// ============================================
// Set 2: Database

[<Interface>]
type IDatabase = 
    abstract Query : string -> string


// ============================================
// Functions using a single dependency via
// the 'env' parameter.
// ============================================

module Log = 

    let debug (env: #ILogger) fmt = 
        Printf.kprintf env.Debug fmt

    let debugDep message = 
        Console.WriteLine ("ERROR: " + message)


module Db = 

    let fetchUser (env: #IDatabase) userId = 
        let user = env.Query userId
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

// IDEA: Copy this before Program.fs
// IDEA: Use implementation from uptream modules
type LiveDependencies(creds: Credentials) = 
    interface ILogger with
        member _.Debug message = 
            Log.debugDep message // NOTE: Upstream module implementation.

        member _.Error message = 
            Console.WriteLine ("ERROR: " + message)
            
    interface IDatabase with
        // Do some stuff with the database
        // What about the connection string?
        member _.Query userId =
            let key = creds.Key  // Passing credentials!
            "user"


let liveCreds = {
    Key = "key"
    Secret = "secret"
}

let live = LiveDependencies liveCreds


let getUser() = fetchUserPlusLogging live "12"

//TODO: Try dummy implentation with my refactored code (also use dummy credentials)
