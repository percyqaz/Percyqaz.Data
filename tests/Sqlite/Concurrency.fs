namespace Percyqaz.Data.Tests.Sqlite

open System
open NUnit.Framework
open Percyqaz.Data.Sqlite

[<TestFixture>]
type Concurrency() =
        
    [<Test>] 
    member this.ConcurrentBatchInsert() =
        let db, conn = Database.in_memory("ConcurrentBatchInsert")
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
        conn.Dispose()

    [<Test>] 
    member this.ConcurrentRoundTripsById() =
        let db, conn = Database.in_memory("ConcurrentRoundTripsById")
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
        conn.Dispose()
    
    [<Test>] 
    member this.ConcurrentRoundTripBatchInsertOnDisk() =
        let db = db_from_new_file "ConcurrentRoundTripBatchInsertOnDisk.db"
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
    member this.ConcurrentRoundTripsByIdOnDisk() =
        let db = db_from_new_file "ConcurrentRoundTripsByIdOnDisk.db"
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