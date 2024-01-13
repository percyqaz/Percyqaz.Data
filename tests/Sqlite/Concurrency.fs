namespace Percyqaz.Data.Tests.Sqlite

open System
open NUnit.Framework
open Percyqaz.Data.Sqlite

[<TestFixture>]
type Concurrency() =
        
    [<Test>] 
    member this.ConcurrentBatchInsert() =
        let db = Database.in_memory()
        db |> Database.create_table Users.TABLE |> expect |> printfn "Table created; %A rows modified"

        let worker i = async {
            let random = Random()
            let users = List.init 100 (fun j ->
                    { 
                        Username = sprintf "User_%i" (i * 100 + j)
                        DateLastSeen = DateTimeOffset.Now.ToUnixTimeSeconds() |> Some
                        Badges = Set.ofList [ "badge1"; "badge2"; "badge3" ]
                        Score = Math.PI * random.NextDouble()
                    }
                )
            db |> Users.INSERT.Batch users |> expect |> printfn "Batch insert %i; %A rows modified" i
        }

        seq { 0 .. 99 } |> Seq.map worker |> Async.Parallel |> Async.Ignore |> Async.RunSynchronously

        let retrieved_users = db |> Users.QUERY_ALL.Execute () |> expect |> Seq.sortBy fst |> Seq.map snd |> List.ofSeq
        Assert.AreEqual(10000, retrieved_users.Length)

    [<Test>] 
    member this.ConcurrentRoundTripsById() =
        let db = Database.in_memory()
        db |> Database.create_table Users.TABLE |> expect |> printfn "Table created; %A rows modified"

        let worker i = async {
            let random = Random()
            let user =
                {
                    Username = sprintf "User_%i" i
                    DateLastSeen = DateTimeOffset.Now.ToUnixTimeSeconds() |> Some
                    Badges = Set.ofList [ "badge1"; "badge2"; "badge3" ]
                    Score = Math.PI * random.NextDouble()
                }
            let user_id = db |> Users.INSERT.ExecuteGetId user |> expect
            printfn "Inserted user %i with id %i" i user_id
            let retrieved_user_by_id = db |> Users.QUERY_BY_ID.Execute user_id |> expect |> Seq.exactlyOne
            Assert.AreEqual(user, retrieved_user_by_id)
        }

        seq { 0 .. 99 } |> Seq.map worker |> Async.Parallel |> Async.Ignore |> Async.RunSynchronously

        let retrieved_users = db |> Users.QUERY_ALL.Execute () |> expect |> Seq.sortBy fst |> Seq.map snd |> List.ofSeq
        Assert.AreEqual(100, retrieved_users.Length)
    
    [<Test>] 
    member this.RoundTripBatchInsertOnDisk() =
        let random = Random()
        let users = List.init 10000 (fun i ->
                { 
                    Username = sprintf "User_%i" i
                    DateLastSeen = DateTimeOffset.Now.ToUnixTimeSeconds() |> Some
                    Badges = Set.ofList [ "badge1"; "badge2"; "badge3" ]
                    Score = Math.PI * random.NextDouble()
                }
            )

        let db = Database.from_file("database.db")
        db |> Database.drop_table_if_exists Users.TABLE |> expect |> printfn "Table dropped if exists; %A rows modified"
        db |> Database.create_table Users.TABLE |> expect |> printfn "Table created; %A rows modified"
        db |> Users.INSERT.Batch users |> expect |> printfn "Batch insert; %A rows modified"

        let retrieved_users = db |> Users.QUERY_ALL.Execute () |> expect |> Seq.sortBy fst |> Seq.map snd |> List.ofSeq
        Assert.AreEqual(10000, retrieved_users.Length)
        Assert.AreEqual(users, retrieved_users)