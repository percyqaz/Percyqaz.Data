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

db
|> Database.insert Users.TABLE (Users.to_row { Username = "Percyqaz"; DateLastSeen = None })
|> printfn "%A"

db
|> Database.insert Users.TABLE (Users.to_row { Username = "Umisen_Yamasen"; DateLastSeen = Some System.DateTime.UtcNow.Ticks })
|> printfn "%A"

db
|> Database.select_all Users.TABLE (fun row -> row.Int64, Users.from_row row)
|> printfn "%A"