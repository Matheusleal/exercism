// based on https://exercism.org/tracks/fsharp/exercises/squeaky-clean/solutions/trash-na-dim
module SqueakyCleanAlternative1

open System

let transform (c: char) : string =
    match c with
    | '-' -> "_"
    | x when Char.IsDigit x -> ""
    | x when Char.IsWhiteSpace x -> ""
    | x when Char.IsUpper x -> $"-{Char.ToLower x}"
    | x when 'α' <= x && x <= 'ω' -> "?"
    | x -> string x

let clean (identifier: string): string =
    String.collect transform identifier