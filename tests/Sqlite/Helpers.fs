namespace Percyqaz.Data.Tests.Sqlite

open Percyqaz.Data
open NUnit.Framework

[<AutoOpen>]
module Helpers =

    let JSON = Json(Json.Settings.Default).WithDefaults()

    let expect = function Ok v -> v | Error err -> failwithf "%A" err