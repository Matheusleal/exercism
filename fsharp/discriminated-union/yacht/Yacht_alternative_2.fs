// https://exercism.org/tracks/fsharp/exercises/yacht/solutions/Chiel-B
module Yacht_alternative_2

type Category = 
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One = 1
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6
    
let score category dice = 
    let sorted = dice |> List.map int |> List.sortDescending
    let counted = sorted |> List.countBy id |> List.sortByDescending snd
    let scoreSingle die: int = dice |> List.filter (fun d -> d = die) |> List.length |> (*) (int die)

    match category with 
        | Ones -> scoreSingle Die.One
        | Twos -> scoreSingle Die.Two
        | Threes -> scoreSingle Die.Three
        | Fours -> scoreSingle Die.Four
        | Fives -> scoreSingle Die.Five
        | Sixes -> scoreSingle Die.Six
        | FullHouse when snd counted.[0] = 3 && snd counted.[1] = 2 -> List.sum sorted
        | FourOfAKind when snd counted.[0] >= 4 -> fst counted.[0] * 4
        | LittleStraight when List.sort sorted = [1..5] -> 30
        | BigStraight when List.sort sorted = [2..6] -> 30
        | Choice -> List.sum sorted
        | Yacht when counted.Length = 1 && snd counted.[0] = 5 -> 50
        | _ -> 0