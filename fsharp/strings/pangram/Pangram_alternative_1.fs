// https://exercism.org/tracks/fsharp/exercises/pangram/solutions/mellson
module PangramAlternative1

module Pangram

let isPangram (input : string) = 
    set [ 'a'..'z' ] - set (input.ToLower()) 
    |> Set.isEmpty