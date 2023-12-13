open Percyqaz.Data.Sqlite

type User = { Id: int; Username: string; DateLastSeen: int64 }

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

let db = Database.from_file "example.sqlite"

db
|> Database.drop_table_if_exists Users.TABLE
|> printfn "%A"

db
|> Database.create_table Users.TABLE
|> printfn "%A"

db
|> Database.exec_with_parameters Users.TABLE.InsertCommandTemplate (fun p -> p.Add(Users.USERNAME, "Percyqaz").Add(Users.DATE_LAST_SEEN, "01/12/2000") )
|> printfn "%A"

db
|> Database.exec_with_parameters Users.TABLE.InsertCommandTemplate (fun p -> p.Add(Users.USERNAME, "Umisen_Yamasen").Add(Users.DATE_LAST_SEEN, "01/12/2000") )
|> printfn "%A"

let reader =
    db
    |> Database.query "SELECT * from [users]"
    |> Result.toOption
    |> Option.get

while reader.Read() do
    for i in 0 .. reader.FieldCount - 1 do
        printfn "%A" reader.[i]