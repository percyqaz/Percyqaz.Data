namespace Percyqaz.Data.Sqlite

open Microsoft.Data.Sqlite

type ColumnType =
    | INTEGER
    | TEXT
    | REAL
    | NUMERIC
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
    static member Numeric(name: string) = { ColumnType = NUMERIC; Name = name; IsNullable = false; IsUnique = false; }

type Table =
    {
        Name: string
        PrimaryKey: Column
        Columns: Column list
    }
    member this.PrimaryKeyIsInteger = this.PrimaryKey.ColumnType = INTEGER

    member this.CreateCommand (if_not_exists: bool) =
        let pk = 
            sprintf "%s %A PRIMARY KEY%s%s" 
                this.PrimaryKey.Name
                this.PrimaryKey.ColumnType
                (if this.PrimaryKey.IsUnique then assert(this.PrimaryKeyIsInteger); " AUTOINCREMENT" else "")
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

    member this.InsertCommandTemplate =
        if this.PrimaryKeyIsInteger then
            sprintf "INSERT INTO [%s] (%s) VALUES ( %s );"
                this.Name
                (this.Columns |> Seq.map _.Name |> String.concat ", ")
                (this.Columns |> Seq.map (fun c -> sprintf "@%s" c.Name) |> String.concat ", ")
        else
            sprintf "INSERT INTO [%s] (%s) VALUES ( %s );"
                this.Name
                (this.PrimaryKey :: this.Columns |> Seq.map _.Name |> String.concat ", ")
                (this.PrimaryKey :: this.Columns |> Seq.map (fun c -> sprintf "@%s" c.Name) |> String.concat ", ")

type CommandParameterHelper(parameters: SqliteParameterCollection) =
    
    member this.Add(col: Column, value: obj) =
        parameters.AddWithValue(col.Name, value) |> ignore
        this

type Database =
    {
        ConnectionString: string
        Connection: SqliteConnection
    }

module Database =

    let from_file(path: string) =
        let connection_string = sprintf "Data Source=%s" path
        let conn = new SqliteConnection(connection_string)
        conn.Open()
        {
            ConnectionString = connection_string
            Connection = conn // todo: reopen connection to db when needed
        }
            
    let exec_with_parameters (command: string) (add_parameters: CommandParameterHelper -> 'T) (db: Database) =
        let c = db.Connection.CreateCommand()
        c.CommandText <- command
        add_parameters <| CommandParameterHelper(c.Parameters) |> ignore
        try
            c.ExecuteNonQuery() |> Ok
        with :? SqliteException as e ->
            Error e.Message

    let exec (command: string) (db: Database) = exec_with_parameters command ignore db

    let query_with_parameters (query: string) (add_parameters: CommandParameterHelper -> 'T) (db: Database) : Result<SqliteDataReader, string> =
        let c = db.Connection.CreateCommand()
        c.CommandText <- query
        add_parameters <| CommandParameterHelper(c.Parameters) |> ignore
        try
            c.ExecuteReader() |> Ok
        with :? SqliteException as e ->
            Error e.Message

    let query (query: string) (db: Database) = query_with_parameters query ignore db

    let create_table (table: Table) (db: Database) = exec (table.CreateCommand false) db
    let create_table_if_not_exists (table: Table) (db: Database) = exec (table.CreateCommand true) db

    let drop_table (table: Table) (db: Database) = exec (table.DropCommand false) db
    let drop_table_if_exists (table: Table) (db: Database) = exec (table.DropCommand true) db