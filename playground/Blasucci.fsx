#r "nuget: Microsoft.Data.Sqlite"
#r "nuget: FsToolkit.ErrorHandling"

open System
open System.Data
open FsToolkit.ErrorHandling


// Types go here...
type Creds = {
    Key: string
    Secret: string
    Url: string
}

type GetConnection = unit -> IDbConnection

[<Interface>]
type IStore = // Database calls
    //abstract GetConnection: unit -> IDbConnection
    abstract GetX: unit -> Async<Result<int,string>>


[<Interface>]
type IWeb = // REST calls
    abstract GetY: unit -> Async<Result<int,string>>


// 1) Pure, don't think about IO at all
module Domain = 
    let add x y z = x + y + z


// 2) Think about IO, but not its implementation
module App = 
    // val env : :> IStore and :> IWeb
    let add env z =
        let store = env :> IStore
        let web   = env :> IWeb

        asyncResult {
            let! x = store.GetX ()
            let! y = web.GetY ()

            return Domain.add x y z
        }


// 3) Implementation of dependencies
module Infra = 

    let getX (getConnection: GetConnection) = 
        async {
            use connection = getConnection ()

            return Ok 3
        }


    let getY (creds: Creds) = 
        async {
            return Ok 4
        }


    type Dependencies (getConnection : GetConnection, creds: Creds) =

        interface IStore with 
            member _.GetX () = 
                getX getConnection

        interface IWeb with
            member _.GetY () = 
                getY creds


// 4) Inject dependencies
module Startup = 
    open Microsoft.Data.Sqlite

    let creds = {
        Key = "key"
        Secret = "secret"
        Url = "url"
    }

    let connStr = "Data Source=hello.db"
    let connection () = new SqliteConnection(connStr) :> IDbConnection

    let deps = Infra.Dependencies(connection, creds)

    let add = App.add deps


Startup.add 3
    |> Async.RunSynchronously
// val it : Result<int,string> = Ok 10


                


    
