module Bob

open System.Text.RegularExpressions

let isSilence (input: string): bool = input.Trim().Length = 0
let isQuestion (input: string): bool = input.Trim().EndsWith("?")
let isYellQuestion (input: string): bool = Regex.IsMatch(input, "^(?:[A-Z\s']+)[?]$")
let isYelling (input: string): bool = Regex.IsMatch(input, "^(?=.*[A-Z])[A-Z0-9\s%^*@#$(),.!^]+$")

let response (input: string): string =
    match input with
    | input when isSilence input -> "Fine. Be that way!"
    | input when isYellQuestion input -> "Calm down, I know what I'm doing!"
    | input when isYelling input -> "Whoa, chill out!"
    | input when isQuestion input -> "Sure."
    | _ -> "Whatever."