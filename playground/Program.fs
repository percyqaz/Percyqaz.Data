open Percyqaz.Data.Sqlite

type User = { Username: string; DateLastSeen: int64 option }

module Users =
    let ID = Column.Integer("Id").Unique
    let USERNAME = Column.Text("Username").Unique
    let DATE_LAST_SEEN = Column.Integer("DateLastSeen").Nullable

    let TABLE =
        {
            Name = "users"
            PrimaryKey = ID
            Columns = 
                [
                    USERNAME
                    DATE_LAST_SEEN
                ]
        }

    let to_row (user: User) =
        fun (p: CommandParameterHelper) -> 
            p
                .Add(USERNAME, user.Username)
                .Add(DATE_LAST_SEEN, user.DateLastSeen)

    let from_row (read: RowReaderHelper) =
        {
            Username = read.String
            DateLastSeen = read.Int64Option
        }

let db = Database.from_file "example.sqlite"

db
|> Database.drop_table_if_exists Users.TABLE
|> printfn "%A"

db
|> Database.create_table Users.TABLE
|> printfn "%A"

let users_to_add =
    seq {
        for i = 0 to 9999 do
            yield { Username = sprintf "User_%i" i; DateLastSeen = Some <| System.Random().NextInt64() }
    }
db
|> Database.batch Users.TABLE.InsertCommandTemplate users_to_add Users.to_row
|> printfn "%A"

db
|> Database.insert Users.TABLE (Users.to_row { Username = "Percyqaz"; DateLastSeen = None })
|> printfn "%A"

db
|> Database.select_all Users.TABLE (fun row -> row.Int64, Users.from_row row)
|> printfn "%A"

db
|> Database.select
    (Users.TABLE.Select(Users.ID, Users.USERNAME, Users.DATE_LAST_SEEN).OrderBy(Users.DATE_LAST_SEEN, true).Limit(10))
    id
    (fun row -> row.Int64, row.String, row.Int64Option)
|> Result.map (List.ofSeq)
|> printfn "%A"