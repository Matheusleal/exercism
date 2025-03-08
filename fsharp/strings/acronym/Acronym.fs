module Acronym
open System

let abbreviate (phrase: string): string = 
    phrase
    |> fun str -> str.Split [|' '; '-'; '_'|]
    |> Array.map (fun x -> if x.Length > 0 then (string x[0]).ToUpper() else "")
    |> String.concat""