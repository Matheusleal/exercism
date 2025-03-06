module Pangram

let isPangram (input: string): bool = 
    let alphabet = "abcdefghijklmnopqrstuvwxyz"
    
    alphabet
    |> Seq.toArray
    |> Array.filter (input.ToLowerInvariant()).Contains
    |> Seq.length = alphabet.Length