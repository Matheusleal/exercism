module ZebraPuzzleTests

open FsUnit.Xunit
open Xunit

open ZebraPuzzle2

[<Fact>]
let ``Resident who drinks water`` () =
    drinksWater |> should equal Norwegian

[<Fact>]
let ``Resident who owns zebra`` () =
    ownsZebra |> should equal Japanese
