let connectionString = "Data Source=./database.db;Version=3;Compress=True"

type User = { Id: int; Username: string; DateLastSeen: int64 }

open Microsoft.Data.Sqlite

let conn = new SqliteConnection(connectionString)
conn.Open()
do
    let cmd = conn.CreateCommand()
    cmd.CommandText <- "CREATE TABLE users ( Id INTEGER PRIMARY KEY, Username TEXT NOT NULL, DateLastSeen DATE NOT NULL );"
    cmd.ExecuteNonQuery() |> printfn "%i"
do
    let cmd = conn.CreateCommand()
    cmd.CommandText <- "SELECT * from dbo.[users]"
    let reader = cmd.ExecuteReader()

//let db = Sql.connect connectionString

//do
//    db
//    |> Sql.query "SELECT * FROM dbo.[Users]"
//    |> Sql.execute(fun r ->
//        {
//            Id = r.int "id"
//            Username = r.string "username"
//            DateLastSeen = r.int64 "date_last_seen"
//        })
//    |> Result.defaultValue []
//    |> List.iter (printfn "%A")