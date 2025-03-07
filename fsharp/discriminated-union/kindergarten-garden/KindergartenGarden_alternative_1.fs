// https://exercism.org/tracks/fsharp/exercises/kindergarten-garden/solutions/ChrisCanCompute
module KindergartenGardenAlternative1

type Plant = | Violets | Clover | Grass | Radishes | Unknown
let children = [ "Alice"; "Bob"; "Charlie"; "David"; "Eve"; "Fred"; "Ginny"; "Harriet"; "Ileana"; "Joseph"; "Kincaid"; "Larry" ]

let plants (diagram : string) student =
    let index = children |> List.findIndex (fun x -> x = student)
    diagram.Split [|'\n'|]
        |> Seq.collect (Seq.skip (index * 2) >> Seq.take 2)
        |> Seq.map (function | 'C' -> Clover | 'G' -> Grass | 'R' -> Radishes | 'V' -> Violets | _ -> Unknown)
        |> List.ofSeq