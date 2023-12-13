open Percyqaz.Data.Sqlite

let connectionString = "Data Source=database.db"

type User = { Id: int; Username: string; DateLastSeen: int64 }

let USER_TABLE =
    {
        Name = "users"
        PrimaryKey = Column.Integer("Id").Unique
        Columns = 
            [
                Column.Text("Username").Unique
                Column.Integer("DateLastSeen").Nullable
            ]
    }

let db = Database.connect connectionString
db
|> Database.drop_table_if_exists USER_TABLE
|> printfn "%A"

db
|> Database.create_table USER_TABLE
|> printfn "%A"

db
|> Database.exec "INSERT INTO [users] (Username, DateLastSeen) VALUES ( 'Percyqaz', '29/05/2000' );"
|> printfn "%A"

db
|> Database.exec "INSERT INTO [users] (Username, DateLastSeen) VALUES ( 'Percyqaz', '29/05/2000' );"
|> printfn "%A"

let reader =
    db
    |> Database.query "SELECT * from [users]"
    |> Result.toOption
    |> Option.get

while reader.Read() do
    for i in 0 .. reader.FieldCount - 1 do
        printfn "%A" reader.[i]