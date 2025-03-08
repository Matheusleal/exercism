// https://exercism.org/tracks/fsharp/exercises/acronym/solutions/damienstanton
module AcronymAlternative1

let abbreviate (phrase: string) =
    [ for w in phrase.Split [|' '; '-'; '_'|] -> w.[0..0].ToUpper() ] 
    |> String.concat ""