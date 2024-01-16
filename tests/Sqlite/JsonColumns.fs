namespace Percyqaz.Data.Tests.Sqlite

open NUnit.Framework
open Percyqaz.Data.Sqlite

[<TestFixture>]
type JsonColumns() =

    [<Test>] 
    member this.RoundTripObjectWithJson() =
        let obj : JsonPropObject = 
            { 
                Name = "My very long object name :)))) 👽️👽️👽️"
                Byte = 255uy
                NestedObject =
                    {
                        Username = "Percyqaz"
                        DateLastSeen = Some 0L
                        Badges = Set.ofList ["A"; "B"; "C"]
                        Score = -0.99 * 0.5
                        List = ["a"; "b"; "c"; "💚"; "e"; "👽️"; "c"; "b"; "A"]
                    }
            }
        
        let db, conn = Database.in_memory("RoundTripObjectWithJson")
        db |> Database.create_table JsonPropObjects.TABLE |> expect |> printfn "Table created; %A rows modified"
        let obj_id = db |> JsonPropObjects.INSERT.ExecuteGetId obj |> expect

        let retrieved_id, retrieved_obj = db |> JsonPropObjects.QUERY_ALL.Execute () |> expect |> Seq.exactlyOne
        Assert.AreEqual(obj, retrieved_obj)
        Assert.AreEqual(obj_id, retrieved_id)
        printfn "%A" retrieved_obj
        conn.Dispose()