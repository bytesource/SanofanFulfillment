#r "nuget: Microsoft.Data.Sqlite"

open System

// Types go here...
type Creds = {
    Key: string
    Secret: string
    Url: string
}

type GetX = unit -> Async<int32>

// 1) Pure, don't think about IO at all
module Domain = 
    let add x y = x + y 


// 2) Think about IO, but not its implementation
module App = 

    let add getX y = 
    // let add (getX: unit -> Async<int32>) y = 
        async {
            let! x = getX()
            return Domain.add x y
        }


// 3) IO implementation 
//    (Implementation for all dependencies that are
//     about to be injected in the 'Startup' module)
module Infra =
    open Microsoft.Data.Sqlite
    let newConnection () = new SqliteConnection ("Data Source=hello.db")
    let getX conn = async { return 7 }


// 4) Inject dependencies
// NOTE: 
// -- Configuration/env is INJECTED only in the 'Startup' module 
// (like any IO) and its 
// -- IMPLEMENTATION is found in the 'Infra' module above.
module Startup = 
    let add y = 
        async {
            use conn = Infra.newConnection ()
            return! App.add 
                (fun () -> Infra.getX conn) 
                y
        }
    

Startup.add 3
|> Async.RunSynchronously
|> printfn "%A"