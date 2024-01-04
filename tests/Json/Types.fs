﻿namespace Percyqaz.Data.Tests.Json

open Percyqaz.Data

type Tuple = int * string
type StructTuple = (struct (bool * unit array))

type Enum =
    | One = 1L
    | Two = 2L
    | Three = 3L

type [<Json.AutoCodec>] Union =
    | Nil
    | One of string
    | Many of Union * float list

type [<Struct>][<Json.AutoCodec>] StructUnion =
    | StNil
    | StOne of string
    | StMany of float list * char

type [<Json.AutoCodec(false)>] Record =
    {
        X: int64
        Y: float32
    }

type [<Json.AutoCodec>] RecordNoDefaults =
    {
        X: int64
        Y: float32
    }

type [<Json.AutoCodec(false)>] RecordWithDefault =
    {
        X: int64
        Y: float32
    }
    static member Default = { X = 64L; Y = 0.3428972214f }

type [<Struct>][<Json.AutoCodec>] StRecord =
    {
        X: int64
        Y: float32
    }

type [<Json.AutoCodec(false)>] RecordPrimitives =
    {
        Reals: float * float32 * decimal
        ShortIntegers: int8 * uint8 * int16 * uint16
        LongIntegers: int32 * uint32 * int64 * uint64 * bigint
        Text: string * char
        Logical: unit * bool
    }