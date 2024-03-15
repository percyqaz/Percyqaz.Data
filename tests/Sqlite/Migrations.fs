namespace Percyqaz.Data.Tests.Sqlite

open System
open NUnit.Framework
open Percyqaz.Data.Sqlite

[<TestFixture>]
type Migrations() =

    [<Test>]
    member this.BasicMigrationSequence() =

        let migration_0 (db: Database) =

            db
            |> Database.create_table Users.TABLE
            |> expect
            |> printfn "Table created; %A rows modified"

        let migration_1 (db: Database) =

            let user: User =
                {
                    Username = "Percyqaz"
                    DateLastSeen = DateTimeOffset.Now.ToUnixTimeSeconds() |> Some
                    Badges = Set.ofList [ "badge1"; "badge2" ]
                    Score = Math.PI * 5.2
                }

            db |> Users.INSERT.ExecuteGetId user |> expect |> ignore

        let migration_2 (db: Database) =

            let user: User =
                {
                    Username = "User_2"
                    DateLastSeen = DateTimeOffset.Now.ToUnixTimeSeconds() |> Some
                    Badges = Set.ofList [ "badge1"; "badge2" ]
                    Score = Math.PI * 5.2
                }

            db |> Users.INSERT.ExecuteGetId user |> expect |> ignore

        let migration_3 (db: Database) =

            Database.exec_raw "UPDATE users SET Username = 'Bananahira' WHERE Username = 'Percyqaz'" db
            |> printfn "%A"

        let db = db_from_new_file ("BasicMigrationSequence.db")

        db |> Database.migrate "CreateUsersTable" migration_0
        db |> Database.migrate "InsertFirstUser" migration_1
        db |> Database.migrate "InsertSecondUser" migration_2
        db |> Database.migrate "RenameFirstUser" migration_3
