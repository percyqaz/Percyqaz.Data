# What's this?

(Under construction)

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

Records must implement a static member `Default`:
```
type MyRecord = {
  A: string
  B: int
} with
  static member Default = { A = "Hello"; B = 5 }
```
And then any missing members are provided by the Default member.
You can mark a field with `[<JsonRequired>]`, if so having it missing from the JSON data will result in an error.

## Supported types:
- Primitive types
- Lists and arrays (F# and System.Collections.Generic)
- Maps and Dictionaries
- Tuples
- Enums
- Records
- F# Unions

## Custom picklers for unsupported types:

POCO types are not supported by themselves.
To add custom support, define a static member `Pickler` for your type that creates a JsonPickler of your type.

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
You can look through Library.fs for more examples on building both simple picklers and for polymorphic types

## Todo list:
- Proper unit tests and benchmarks
- Proper example code and some cleanup for readability
