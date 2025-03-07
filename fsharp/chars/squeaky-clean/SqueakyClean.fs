module SqueakyClean

let lowerLetters: int list = [97..122]
let upperLetters: int list = [65..90]
let greekUpper: int list = [880..939]
let greekLower: int list = [940..975]
let latinExtendedA: int list = [256..383]

let transform (c: char) : string =
    let toLower (letter:string): string = $"-{letter.ToLower()}"

    int c
    |> function 
        | _ when c = '-' -> "_" 
        | x when List.contains x latinExtendedA -> string c |> toLower
        | x when List.contains x greekLower -> "?"
        | x when List.contains x greekUpper -> string c |> toLower
        | x when List.contains x lowerLetters -> string c
        | x when List.contains x upperLetters -> string c |> toLower
        | x when x < int upperLetters.Head -> ""
        | _ -> c |> string

     
let clean (identifier: string): string =
    String.collect transform identifier
