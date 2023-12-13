namespace Percyqaz.Data.Sqlite

open Microsoft.Data.Sqlite

type ColumnType =
    | INTEGER
    | TEXT
    | REAL
    | BLOB

type Column =
    {
        ColumnType: ColumnType
        Name: string
        IsNullable: bool
        /// for INTEGER PRIMARY KEYs, this represents AUTOINCREMENT
        IsUnique: bool
    }
    member this.Unique = { this with IsUnique = true }
    member this.Nullable = { this with IsNullable = true }

    static member Text(name: string) = { ColumnType = TEXT; Name = name; IsNullable = false; IsUnique = false; }
    static member Integer(name: string) = { ColumnType = INTEGER; Name = name; IsNullable = false; IsUnique = false; }
    static member Real(name: string) = { ColumnType = REAL; Name = name; IsNullable = false; IsUnique = false; }
    static member Blob(name: string) = { ColumnType = BLOB; Name = name; IsNullable = false; IsUnique = false; }

type Table =
    {
        Name: string
        PrimaryKey: Column
        Columns: Column list
    }
    member this.CreateCommand (if_not_exists: bool) =
        let pk = 
            sprintf "%s %A PRIMARY KEY%s%s" 
                this.PrimaryKey.Name
                this.PrimaryKey.ColumnType
                (if this.PrimaryKey.IsUnique then assert(this.PrimaryKey.ColumnType = INTEGER); " AUTOINCREMENT" else "")
                (if this.PrimaryKey.IsNullable then "" else " NOT NULL")
        let cols =
            this.Columns 
            |> Seq.map (fun c ->
                sprintf "%s %A%s%s" 
                    c.Name
                    c.ColumnType
                    (if c.IsUnique then " UNIQUE" else "")
                    (if c.IsNullable then "" else " NOT NULL")
            )
            |> String.concat ", "
        if if_not_exists then
            sprintf "CREATE TABLE [%s] IF NOT EXISTS ( %s, %s );" this.Name pk cols
        else sprintf "CREATE TABLE [%s] ( %s, %s );" this.Name pk cols
    member this.DropCommand (if_exists: bool) =
        if if_exists then 
            sprintf "DROP TABLE IF EXISTS [%s];" this.Name
        else sprintf "DROP TABLE [%s];" this.Name

type Database =
    {
        ConnectionString: string
        Connection: SqliteConnection
    }

module Database =

    let connect(str: string) =
        let conn = new SqliteConnection(str)
        conn.Open()
        {
            ConnectionString = str
            Connection = conn
        }

    let exec (command: string) (db: Database) =
        let c = db.Connection.CreateCommand()
        c.CommandText <- command
        try
            c.ExecuteNonQuery() |> Ok
        with :? SqliteException as e ->
            Error e.Message

    let query (query: string) (db: Database) : Result<SqliteDataReader, string> =
        let c = db.Connection.CreateCommand()
        c.CommandText <- query
        try
            c.ExecuteReader() |> Ok
        with :? SqliteException as e ->
            Error e.Message

    let create_table (table: Table) (db: Database) = exec (table.CreateCommand false) db
    let create_table_if_not_exists (table: Table) (db: Database) = exec (table.CreateCommand true) db

    let drop_table (table: Table) (db: Database) = exec (table.DropCommand false) db
    let drop_table_if_exists (table: Table) (db: Database) = exec (table.DropCommand true) db