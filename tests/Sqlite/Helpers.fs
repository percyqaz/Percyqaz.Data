namespace Percyqaz.Data.Tests.Sqlite

open Percyqaz.Data
open Percyqaz.Data.Sqlite

[<AutoOpen>]
module Helpers =

    let JSON = Json(Json.Settings.Default).WithDefaults()

    let expect =
        function
        | Ok v -> v
        | Error err -> failwithf "%A" err

    let db_from_new_file (name: string) =
        if System.IO.File.Exists name then
            System.IO.File.Delete name

        Database.from_file name

type User =
    {
        Username: string
        DateLastSeen: int64 option
        Badges: Set<string>
        Score: float
    }

module Users =

    let TABLE =
        {
            Name = "users"
            PrimaryKey = Column.Integer("Id").Unique
            Columns =
                [
                    Column.Text("Username").Unique
                    Column.Integer("DateLastSeen").Nullable
                    Column.Text("Badges")
                    Column.Real("Score")
                ]
        }

    let INSERT: NonQuery<User> =
        {
            SQL = TABLE.INSERT
            Parameters =
                [
                    "@Username", SqliteType.Text, -1
                    "@DateLastSeen", SqliteType.Integer, 8
                    "@Badges", SqliteType.Text, -1
                    "@Score", SqliteType.Real, 8
                ]
            FillParameters =
                (fun (p: CommandParameterHelper) (user: User) ->
                    p.String user.Username
                    p.Int64Option user.DateLastSeen
                    p.Json JSON user.Badges
                    p.Float64 user.Score
                )
        }

    let QUERY_ALL: Query<unit, int64 * User> =
        { Query.without_parameters () with
            SQL =
                """
            SELECT *
            FROM [users];
            """
            Read =
                (fun (read: RowReaderHelper) ->
                    read.Int64,
                    {
                        Username = read.String
                        DateLastSeen = read.Int64Option
                        Badges = read.Json JSON
                        Score = read.Float64
                    }
                )
        }

    let QUERY_BY_ID: Query<int64, User> =
        {
            SQL =
                """
            SELECT *
            FROM [users] 
            WHERE [id] = @Id;
            """
            Parameters = [ "@Id", SqliteType.Integer, 8 ]
            FillParameters = (fun (p: CommandParameterHelper) (id: int64) -> p.Int64 id)
            Read =
                (fun (read: RowReaderHelper) ->
                    read.Int64 |> ignore

                    {
                        Username = read.String
                        DateLastSeen = read.Int64Option
                        Badges = read.Json JSON
                        Score = read.Float64
                    }
                )
        }

    let QUERY_RECENTLY_SEEN: Query<unit, int64 * User> =
        {
            SQL =
                """
            SELECT *
            FROM [users]
            ORDER BY [DateLastSeen] DESC
            LIMIT 10;
            """
            Parameters = []
            FillParameters = fun _ _ -> ()
            Read =
                (fun (read: RowReaderHelper) ->
                    read.Int64,
                    {
                        Username = read.String
                        DateLastSeen = read.Int64Option
                        Badges = read.Json JSON
                        Score = read.Float64
                    }
                )
        }

[<Json.AutoCodec>]
type NestedComplexObject =
    {
        Username: string
        DateLastSeen: int64 option
        Badges: Set<string>
        Score: float
        List: string list
    }

type JsonPropObject =
    {
        Name: string
        Byte: byte
        NestedObject: NestedComplexObject
    }

module JsonPropObjects =

    let TABLE =
        {
            Name = "json_prop_objects"
            PrimaryKey = Column.Integer("Id").Unique
            Columns =
                [
                    Column.Text("Name").Unique
                    Column.Integer("Byte")
                    Column.Text("NestedObject")
                ]
        }

    let INSERT: NonQuery<JsonPropObject> =
        {
            SQL = TABLE.INSERT
            Parameters =
                [
                    "@Name", SqliteType.Text, -1
                    "@Byte", SqliteType.Integer, 1
                    "@NestedObject", SqliteType.Text, -1
                ]
            FillParameters =
                (fun (p: CommandParameterHelper) (obj: JsonPropObject) ->
                    p.String obj.Name
                    p.Byte obj.Byte
                    p.Json JSON obj.NestedObject
                )
        }

    let QUERY_ALL: Query<unit, int64 * JsonPropObject> =
        { Query.without_parameters () with
            SQL =
                """
            SELECT *
            FROM [json_prop_objects];
            """
            Read =
                (fun (read: RowReaderHelper) ->
                    read.Int64,
                    {
                        Name = read.String
                        Byte = read.Byte
                        NestedObject = read.Json JSON
                    }
                )
        }
