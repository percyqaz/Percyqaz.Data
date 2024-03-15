open Percyqaz.Data.Sqlite

type User =
    {
        Username: string
        DateLastSeen: int64 option
    }

let db = Database.from_file "example.sqlite"

module Users =

    let TABLE =
        {
            Name = "users"
            PrimaryKey = Column.Integer("Id").Unique
            Columns = [ Column.Text("Username").Unique; Column.Integer("DateLastSeen").Nullable ]
        }

    let ALL: Query<unit, int64 * User> =
        {
            SQL =
                """
            SELECT [Id], [Username], [DateLastSeen]
            FROM [users];
            """
            Parameters = []
            FillParameters = fun _ _ -> ()
            Read =
                (fun (read: RowReaderHelper) ->
                    read.Int64,
                    {
                        Username = read.String
                        DateLastSeen = read.Int64Option
                    }
                )
        }

    let BY_ID: Query<int64, User> =
        {
            SQL =
                """
            SELECT [Username], [DateLastSeen]
            FROM [users] 
            WHERE [id] = @Id;
            """
            Parameters = [ "@Id", SqliteType.Integer, 8 ]
            FillParameters = (fun (p: CommandParameterHelper) (id: int64) -> p.Add id)
            Read =
                (fun (read: RowReaderHelper) ->
                    {
                        Username = read.String
                        DateLastSeen = read.Int64Option
                    }
                )
        }

    let INSERT: NonQuery<User> =
        {
            SQL =
                """
            INSERT INTO [users] (Username, DateLastSeen)
            VALUES ( @Username, @DateLastSeen );
            """
            Parameters = [ "@Username", SqliteType.Text, 255; "@DateLastSeen", SqliteType.Integer, 255 ]
            FillParameters =
                (fun (p: CommandParameterHelper) (user: User) ->
                    p.Add user.Username
                    p.Option user.DateLastSeen
                )
        }

    let RECENTLY_SEEN: Query<unit, int64 * User> =
        {
            SQL =
                """
            SELECT [Id], [Username], [DateLastSeen]
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
                    }
                )
        }

db |> Database.drop_table_if_exists Users.TABLE |> printfn "%A"

db |> Database.create_table Users.TABLE |> printfn "%A"

Users.INSERT.ExecuteGetId
    {
        Username = "Percyqaz"
        DateLastSeen = None
    }
    db
|> printfn "%A"

Users.BY_ID.Execute 1L db |> printfn "%A"

let users_to_add =
    seq {
        for i = 0 to 9999 do
            yield
                {
                    Username = sprintf "User_%i" i
                    DateLastSeen = Some <| System.Random().NextInt64()
                }
    }

Users.INSERT.Batch users_to_add db |> printfn "%A"

Users.RECENTLY_SEEN.Execute () db |> printfn "%A"

Users.ALL.Execute () db |> printfn "%A"
