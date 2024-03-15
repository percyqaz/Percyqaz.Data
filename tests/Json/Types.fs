namespace Percyqaz.Data.Tests.Json

open Percyqaz.Data

type Tuple = int * string
type StructTuple = (struct (bool * unit array))

type Enum =
    | One = 1L
    | Two = 2L
    | Three = 3L

[<Json.AutoCodec>]
type Union =
    | Nil
    | One of string
    | Many of Union * float list

[<Struct>]
[<Json.AutoCodec>]
type StructUnion =
    | StNil
    | StOne of string
    | StMany of float list * char

[<Json.AutoCodec(false)>]
type Record = { X: int64; Y: float32 }

[<Json.AutoCodec>]
type RecordNoDefaults = { X: int64; Y: float32 }

[<Json.AutoCodec(false)>]
type RecordWithDefault =
    {
        X: int64
        Y: float32
    }
    static member Default = { X = 64L; Y = 0.3428972214f }

[<Struct>]
[<Json.AutoCodec>]
type StRecord = { X: int64; Y: float32 }

[<Json.AutoCodec(false)>]
type RecordPrimitives =
    {
        Reals: float * float32 * decimal
        ShortIntegers: int8 * uint8 * int16 * uint16
        LongIntegers: int32 * uint32 * int64 * uint64 * bigint
        Text: string * char
        Logical: unit * bool
    }
