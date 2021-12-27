#r "nuget: Microsoft.Data.Sqlite"

open System


// Types go here...
type Creds = {
    Key: string
    Secret: string
    Url: string
}


// Interface declaration
// NOTE: Wherever we put this, 
//       all types need to be defined.
[<Interface>] 
type ISql = 
    abstract GetX: unit -> Async<int32>
    abstract GetCustomerId: int -> Async<Result<int,_>>

[<Interface>]
type ILog = 
    abstract Debug: string -> unit


// 1) Pure, don't think about IO at all
module Domain = 
    let add x y = x + y 


// 2) Think about IO, but not its implementation
//    (USE the dependencies without 
//     worrying about their implemtation)
module App = 
    let add (sql: ISql) y =
    // let add (env: ISql) y =
        async {
            let! x = sql.GetX()
            return Domain.add x y
        }

    let addWithLog (sql: ISql) (log: ILog) y =
         log.Debug $"About to add number {y}"
         add sql y


// 3) Implementation of dependencies
//    Provide implementation for each dependency,
//    add additional parameters if needed.
//    (These implementations will be added to the interfaces)
module Infra = 
    open Microsoft.Data.Sqlite
    let newConnection () = new SqliteConnection("Data Source=hello.db")

    let getX conn (creds: Creds) = async { return 7 }
    let getY conn (creds: Creds) num = async { return Ok 3 }

    let debug message = 
        printfn $"DEBUG: %s{message}"


    type Logging () = 
        interface ILog with 
            member _.Debug message = 
                debug message


    type Sql (conn, creds: Creds)=
        interface ISql with 
            member _.GetX () = 
                getX conn creds

            member _.GetCustomerId num = 
                getY conn creds num


// 4) Inject dependencies
module Startup = 

    let creds = {
        Key = "key"
        Secret = "secret"
        Url = "url"
    }
  
    
    // Set up dependencies
    let conn = Infra.newConnection ()
    let logging = Infra.Logging ()
    let sql = Infra.Sql (conn, creds)


    // Partially apply to get rid of the deps parameters
    let add = App.addWithLog sql logging


Startup.add 3
|> Async.RunSynchronously