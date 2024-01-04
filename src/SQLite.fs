namespace Percyqaz.Data.Sqlite

open System
open Microsoft.Data.Sqlite
open Percyqaz.Data

type SqliteType = Microsoft.Data.Sqlite.SqliteType

type Column =
    {
        ColumnType: SqliteType
        Name: string
        IsNullable: bool
        /// for INTEGER PRIMARY KEYs, this represents AUTOINCREMENT
        IsUnique: bool
    }
    member this.Unique = { this with IsUnique = true }
    member this.Nullable = { this with IsNullable = true }

    static member Text(name: string) = { ColumnType = SqliteType.Text; Name = name; IsNullable = false; IsUnique = false; }
    static member Integer(name: string) = { ColumnType = SqliteType.Integer; Name = name; IsNullable = false; IsUnique = false; }
    static member Real(name: string) = { ColumnType = SqliteType.Real; Name = name; IsNullable = false; IsUnique = false; }
    static member Blob(name: string) = { ColumnType = SqliteType.Blob; Name = name; IsNullable = false; IsUnique = false; }

type Table =
    {
        Name: string
        PrimaryKey: Column
        Columns: Column list
    }
    member this.PrimaryKeyIsInteger = this.PrimaryKey.ColumnType = SqliteType.Integer

    member this.CreateCommand (if_not_exists: bool) =
        let pk = 
            sprintf "%s %s PRIMARY KEY%s%s" 
                this.PrimaryKey.Name
                (this.PrimaryKey.ColumnType.ToString().ToUpper())
                (if this.PrimaryKey.IsUnique then assert(this.PrimaryKeyIsInteger); " AUTOINCREMENT" else "")
                (if this.PrimaryKey.IsNullable then "" else " NOT NULL")
        let cols =
            this.Columns 
            |> Seq.map (fun c ->
                sprintf "%s %s%s%s" 
                    c.Name
                    (c.ColumnType.ToString().ToUpper())
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

    member this.Select([<ParamArray>] columns: Column array) =
        {
            Table = this
            Columns = columns |> Array.map (fun c -> sprintf "[%s]" c.Name) |> List.ofArray
            Filter = None
            Order = None
            Group = None
            Page = None
        }

    member this.SelectAll = this.Select(this.PrimaryKey :: this.Columns |> Array.ofList)

and SelectQuery =
    {
        Table: Table
        Columns: string list
        Filter: string option
        Group: string option
        Order: (string * bool) option
        Page: (int * int) option
    }
    member this.Column (id: string) = { this with Columns = this.Columns @ [id] }
    member this.Where (id: string) = 
        if this.Filter.IsSome then failwithf "Already filtering by %A" this.Filter.Value
        { this with Filter = Some id }
    member this.GroupBy (col: Column) = 
        if this.Group.IsSome then failwithf "Already grouping by %A" this.Group.Value
        { this with Group = Some col.Name }
    member this.OrderBy (col: Column, desc: bool) =
        if this.Order.IsSome then failwithf "Already ordering by %A" this.Order.Value
        { this with Order = Some (col.Name, desc) }
    member this.Limit (count: int, page: int) = { this with Page = Some (count, page) }
    member this.Limit (count: int) = this.Limit (count, 0)

    member this.Command =
        sprintf "SELECT %s\nFROM [%s]%s%s%s%s;" 
            (String.concat ", " this.Columns)
            this.Table.Name
            (match this.Filter with Some s -> sprintf "\nWHERE %s" s | None -> "")
            (match this.Group with Some s -> sprintf "\nGROUP BY %s" s | None -> "")
            (match this.Order with Some (s, desc) -> sprintf "\nORDER BY %s %s" s (if desc then "DESC" else "ASC") | None -> "")
            (match this.Page with Some (count, page) -> sprintf "\nLIMIT %i OFFSET %i" count (count * page) | None -> "")

type CommandParameterHelper(count: int, parameters: SqliteParameterCollection) =
    let mutable col = -1

    member private this.Column =
        col <- col + 1
        col

    member this.Next() =
        assert(col + 1 = count)
        col <- -1

    member this.Option(value: 'T option) =
        match value with
        | Some v -> this.Add(v)
        | None -> this.Null()
        
    member this.Null() =
        parameters.[this.Column].Value <- DBNull.Value

    member this.Add(value: obj) =
        parameters.[this.Column].Value <- value

type RowReaderHelper(reader: SqliteDataReader) =
    let mutable col = -1

    member this.Next() =
        assert(col < 0 || col + 1 = reader.FieldCount)
        col <- -1

    member private this.Column =
        col <- col + 1
        col

    member inline private this.Option<'T> (method: int -> 'T) = 
        let c = this.Column
        if reader.IsDBNull c then None 
        else Some (method c)
    
    member this.Boolean = reader.GetBoolean this.Column
    member this.BooleanOption = this.Option reader.GetBoolean

    member this.Int8 = reader.GetByte this.Column
    member this.Int8Option = this.Option reader.GetByte

    member this.Int16 = reader.GetInt16 this.Column
    member this.Int16Option = this.Option reader.GetInt16

    member this.Int32 = reader.GetInt32 this.Column
    member this.Int32Option = this.Option reader.GetInt32
    
    member this.Int64 = reader.GetInt64 this.Column
    member this.Int64Option = this.Option reader.GetInt64
    
    member this.Float32 = reader.GetFloat this.Column
    member this.Float32Option = this.Option reader.GetFloat

    member this.Float64 = reader.GetDouble this.Column
    member this.Float64Option = this.Option reader.GetDouble

    member this.Decimal = reader.GetDecimal this.Column
    member this.DecimalOption = this.Option reader.GetDecimal

    member this.String = reader.GetString this.Column
    member this.StringOption = this.Option reader.GetString
    
    member this.Stream = reader.GetStream this.Column
    member this.StreamOption = this.Option reader.GetStream
    
    member this.Json<'T>(json: Json) = json.FromStream ("sqlite", reader.GetStream this.Column)
        
type Database =
    {
        ConnectionString: string
        mutable Connection: SqliteConnection option
    }
    member this.Connect() =
        match this.Connection with
        | None ->
            let connection = new SqliteConnection(this.ConnectionString)
            connection.Open()
            this.Connection <- Some connection
            connection
        | Some connection ->
            if not (connection.State.HasFlag Data.ConnectionState.Open) then
                printfn "%A" connection.State
                connection.Open()
            connection

type Query<'Parameters, 'Result> =
    {
        SQL: string
        Parameters: (string * SqliteType * int) list
        FillParameters: CommandParameterHelper -> 'Parameters -> unit
        Read: RowReaderHelper -> 'Result
    }
    member this.CreateParameters(command: SqliteCommand) =
        for p, ty, size in this.Parameters do
            command.Parameters.Add(new SqliteParameter(p, ty, size)) |> ignore

type NonQuery<'Parameters> = 
    {
        SQL: string
        Parameters: (string * SqliteType * int) list
        FillParameters: CommandParameterHelper -> 'Parameters -> unit
    }
    member this.CreateParameters(command: SqliteCommand) =
        for p, ty, size in this.Parameters do
            command.Parameters.Add(new SqliteParameter(p, ty, size)) |> ignore

module Query =

    let exec (query: Query<'Parameter, 'Result>) (value: 'Parameter) (db: Database) : Result<'Result seq, string> =
        let command = new SqliteCommand(query.SQL, db.Connect())
        query.CreateParameters command

        let helper = CommandParameterHelper(query.Parameters.Length, command.Parameters)
        query.FillParameters helper value
        helper.Next()

        try
            let sql_reader = command.ExecuteReader()
            let reader = RowReaderHelper(sql_reader)
            seq {
                while sql_reader.Read() do
                    yield query.Read reader
                    reader.Next()
            } |> Ok
        with :? SqliteException as e ->
            Error e.Message

module NonQuery =

    let exec (query: NonQuery<'Parameter>) (value: 'Parameter) (db: Database) : Result<int, string> =
        let command = new SqliteCommand(query.SQL, db.Connect())
        query.CreateParameters command

        let helper = CommandParameterHelper(query.Parameters.Length, command.Parameters)
        query.FillParameters helper value
        helper.Next()

        try
            command.ExecuteNonQuery() |> Ok
        with :? SqliteException as e ->
            Error e.Message
            
    let exec_with_id (query: NonQuery<'Parameter>) (value: 'Parameter) (db: Database) : Result<int64, string> =
        let command = new SqliteCommand(query.SQL + " SELECT last_insert_rowid();", db.Connect())
        query.CreateParameters command
            
        let helper = CommandParameterHelper(query.Parameters.Length, command.Parameters)
        query.FillParameters helper value
        helper.Next()
            
        try
            command.ExecuteScalar() |> unbox |> Ok
        with :? SqliteException as e ->
            Error e.Message

    let batch (query: NonQuery<'Parameter>) (values: 'Parameter seq) (db: Database) : Result<int, string> =
        if Seq.isEmpty values then Ok 0 else

        let connection = db.Connect()
        use transaction = connection.BeginTransaction()
        let command = new SqliteCommand(query.SQL, connection, transaction)
        query.CreateParameters command
        command.Prepare()

        let mutable affected_rows = 0
        let helper = CommandParameterHelper(query.Parameters.Length, command.Parameters)
        try
            for value in values do
                
                query.FillParameters helper value
                helper.Next()
                affected_rows <- affected_rows + command.ExecuteNonQuery()
            transaction.Commit()
            Ok affected_rows
        with :? SqliteException as e ->
            Error e.Message

module Database =

    let exec_raw (sql: string) (db: Database) : Result<int, string> =
        let command = db.Connect().CreateCommand()
        command.CommandText <- sql
        try
            command.ExecuteNonQuery() |> Ok
        with :? SqliteException as e ->
            Error e.Message

    let create_table (table: Table) (db: Database) = exec_raw (table.CreateCommand false) db
    let create_table_if_not_exists (table: Table) (db: Database) = exec_raw (table.CreateCommand true) db

    let drop_table (table: Table) (db: Database) = exec_raw (table.DropCommand false) db
    let drop_table_if_exists (table: Table) (db: Database) = exec_raw (table.DropCommand true) db

    let private init (db: Database) =
        exec_raw "PRAGMA encoding = 'UTF-8'" db 
        |> function Ok _ -> () | Error e -> failwithf "Unexpected error %s" e
        db
    
    let from_file(path: string) =
        let connection_string = sprintf "Data Source=%s" path
        {
            ConnectionString = connection_string
            Connection = None
        } |> init

type Query<'P,'R> with 
    member this.Execute (value: 'P) (db: Database) = Query.exec this value db

type NonQuery<'P> with 
    member this.Execute (value: 'P) (db: Database) = NonQuery.exec this value db
    member this.ExecuteGetId (value: 'P) (db: Database) = NonQuery.exec_with_id this value db
    member this.Batch (values: 'P seq) (db: Database) = NonQuery.batch this values db