## Percyqaz.Data.Sqlite

Documentation is sparse since the person reading this is likely to be ME and I only need enough notes for myself

----

### Step 1: Create a database object
```fsharp
// either from file
let db = Database.from_file(YOUR_PATH_HERE)

// or in memory (particularly used for unit testing)
let db, connection = Database.in_memory(MEANINGFUL_IDENTIFIER_STRING)

// the returned connection when constructing in memory is for you to keep open and then dispose when you are done using it
// the database ceases to exist when the last connection is closed
```

### Step 2: Model your tables and queries
```fsharp
type User =
	{
		Username: string
		OtherProperty: int64 option
	}
	
module Users =

	let TABLE = 
		{
			Name = "users"
			PrimaryKey = Column.Integer("Id").Unique
			Columns = 
				[
					Column.Integer("OtherProperty").Nullable
				]
		}
		
	let INSERT : NonQuery<User> =
		{
			SQL = TABLE.INSERT // table objects have helper methods to generate the SQL for you
			Parameters =
				[
					"@Username", SqliteType.Text, -1
					"@OtherProperty", SqliteType.Integer, 8
				]
			FillParameters = (fun p user ->
				p.Add user.Username
				p.Option user.OtherProperty
			)
		}
		
	let QUERY_BY_ID : Query<int64, User> =
		{
			SQL = """SELECT * FROM [users] WHERE [Id] = @Id;"""
			Parameters = 
				[
					"@Id", SqliteType.Integer, 8
				]
			FillParameters = (fun p id -> p.Add id)
			Read = (fun read ->
				read.Int64 |> ignore // discard the id
				{
					Username = read.String
					DateLastSeen = read.Int64Option
				}
			)
		}
```

### Step 3: In action
```fsharp
let db = Database.from_file("example.db")

match Database.create_table Users.TABLE db with
| Ok rows_affected -> printfn "table was created, %i rows affected" rows_affected
| Error e -> failwithf "Error creating table: %s" e

let user = { Username = "Percyqaz"; OtherProperty = Some 123 }

match Users.INSERT.Execute user db with
| Ok rows_affected -> printfn "user was inserted, %i rows affected" rows_affected
| Error e -> failwithf "Error inserting user: %s" e

// OR to get the id of the user that was just inserted

match Users.INSERT.ExecuteGetId user db with
| Ok inserted_id -> printfn "user was inserted with id %i" inserted_id
| Error e -> failwithf "Error inserting user: %s" e

// querying by ID

let id = 1L

match Users.QUERY_BY_ID.Execute id db with
| Ok user :: [] -> printfn "retrieved single user matching requested id: %A" user
| Ok xs -> failwithf "Expected single user from query, got: %A" xs
| Error e -> failwithf "Error querying user by ID: %s" e
```

### Step 4: Migrations
On program startup, run migrations to apply changes to a database if that step has not already been run
```fsharp
let db = Database.from_file("example.db")

Database.migrate 
	"CreateInitialTables"
	(fun db -> Database.create_table Users.TABLE db)
	db
	
Database.migrate 
	"MoveDataToAnotherTable"
	(fun db ->
		// create another table
		// fetch data from users table
		// transform it in some way, using F# code
		// batch insert into new table
	)
	db
```

Future features: Transactions outside of batch-repeating the same non-query with different parameters

### Step 5
Unit test all your queries and tables by running them against in-memory databases before using in production code