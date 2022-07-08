namespace Percyqaz.Json.Tests

type Tuple = int * string
type StructTuple = (struct (bool * unit array))

type Union =
    | None
    | One of string
    | Many of Union * float list

type [<Struct>] StructUnion =
    | StNone
    | StOne of string
    | StMany of float list * char

type Record =
    {
        X: int64
        Y: float32
    }

type [<Struct>] StRecord =
    {
        X: int64
        Y: float32
    }

type RecordPrimitives =
    {
        Reals: float * float32 * decimal
        ShortIntegers: int8 * uint8 * int16 * uint16
        LongIntegers: int32 * uint32 * int64 * uint64 * bigint
        Text: string * char
        Logical: unit * bool
    }