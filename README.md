# What's this?

Yet another JSON library, specificially for my F# JSON needs which were not met by JSON.Net or Chiron

## How it works
- JSON is parsed with FParsec into an abstract syntax tree
- Mappings are created and cached using TypeShape for each type to transform back and forth between JSON trees and data

## Particular features
- Fail-safe: The mappings can fail midway and still recover because parsing is already done
- Version tolerant: The mappings can handle variations in JSON representations/changes to records without much fuss
- Null-safe for F# types: Record default values must be provided

## Get started
```
open Percyqaz.Json

//deserialise a type from string
let str = ...
let deserializedOutput = Json.fromString<'T>(str)

//or from a file
let filepath = ...
let deserializedOutput = Json.fromFile<'T>(filepath)

//serialization is just as simple
let str = Json.toString(...)
```
### Records
Records can implement a static member `Default` providing an instance of the record, in which case:
- All members are **optional** unless marked with the `Json.Required` attribute
- The default data is used where it is not provided by the JSON data.
```
type MyRecord = {
  A: string
  [<Json.Required>]
  B: int
} with
  static member Default = { A = "Hello"; B = 5 }
```
**If you don't implement a `Default` instance,** then all members are must be provided by the JSON data.

Either way, Records never end up having null values / being non F#-friendly when parsed.

## Supported types:
- Primitive types
- DateTimeand DateTimeOffset (ISO 8601)
- Lists and arrays (F# and System.Collections.Generic)
- Maps and Dictionaries
- Tuples
- Enums
- Records
- F# Unions

## Custom picklers for unsupported types:

POCO types are not supported by themselves.
To add custom support, define a static member `Pickler` for your type that creates a JsonPickler of your type.

Beware - You must take care to explicitly mark its type as `Json.Mapping.JsonPickler<TYPE HERE>` - using `Json.Mapping.mkPickler` to build your pickler is recommended.

As it currently stands, **derived classes cannot use the inherited pickler of the base class and must implement another.**
```
open Percyqaz.Json

type Example<'T>(defaultValue: 'T, value: 'T) =
    member this.Value = value
    member this.DefaultValue = defaultValue
    static member Pickler: Json.Mapping.JsonPickler<Example<'T>> =
        let tP = Json.Mapping.getPickler<'T>()
        Json.Mapping.mkPickler
            (fun (o: Example<'T>) -> tP.Encode(o.Value))
            (fun (o: Example<'T>) json -> tP.Decode(o.DefaultValue)(json) |> Json.JsonResult.map (fun v -> POCOTest(o.DefaultValue, v)))
```
You can look through Library.fs for more examples on building both simple picklers and for polymorphic types. (More documentation may come)

## Todo list:
- Proper unit tests and benchmarks
- More examples and documentation
- Proper example code and some cleanup for readability
- Special cases and options for things like DateTime format, byte arrays as base64, etc
