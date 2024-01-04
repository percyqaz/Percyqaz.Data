namespace Percyqaz.Data.Tests.Json

open NUnit.Framework
open Percyqaz.Data

type Setting =
    {
        Set: int -> unit
        Get: unit -> int
    }
    static member Make (i: int) =
        let mutable i = i
        { Set = (fun x -> printfn "Changing from %i to %i" i x; i <- x); Get = fun () -> i }

[<Json.AutoCodec>]
type SettingRecord = 
    {
        S: Setting
    }
    static member Default = { S = Setting.Make 100 }

type SettingCodec() =
    inherit Json.Codec<Setting>()
    override this.To (ctx: Json.Context) =
        let cdc = ctx.GetCodec<int>()
        fun s -> s.Get() |> cdc.To
    override this.From (ctx: Json.Context) =
        let cdc = ctx.GetCodec<int>()
        fun s json -> s.Set(cdc.From (s.Get()) json); s
    override this.Default (ctx: Json.Context) =
        fun () -> Setting.Make 0

type FloatOverride() =
    inherit Json.Codec<float>()
    override this.To (ctx: Json.Context) =
        fun f -> JSON.Number (f.ToString())
    override this.From (ctx: Json.Context) =
        fun f json -> f
    override this.Default (ctx: Json.Context) =
        fun () -> printfn "Calling custom float default"; -1.0

type ExternalRecord =
    { A: int; B: int } with static member Default = { A = 10; B = 20 }

type ExternalUnion =
    | A of int | B of int

[<TestFixture>]
type ``5: Custom Codec Implementations``() =
        
    let env = 
        Json({ Json.Settings.Default with FormatExpandObjects = false })
            .WithCodec<FloatOverride>()
            .WithDefaults()
            .WithCodec<SettingCodec>()
            .WithAutoCodec<ExternalUnion>()
            .WithAutoCodec<ExternalRecord>(false)

    [<Test>]
    member this.SettingCustomCodec() =
        match env.FromString<SettingRecord> "{\"S\": 99}" with
        | Ok v -> Assert.AreEqual(99, v.S.Get())
        | Error err -> Assert.Fail(sprintf "Unexpected error while converting from string: %O" err)

    [<Test>]
    member this.FloatCustomCodec() =
        Assert.AreEqual(-1.0, env.Default<float>())

    [<Test>]
    member this.ExternalRecordAutoCodec() =
        Helpers.round_trip env { ExternalRecord.Default with A = 100 }
    
    [<Test>]
    member this.ExternalUnionAutoCodec() =
        Helpers.round_trip env (B 100)