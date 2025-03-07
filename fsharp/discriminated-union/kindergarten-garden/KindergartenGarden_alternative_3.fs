// https://exercism.org/tracks/fsharp/exercises/kindergarten-garden/solutions/jrr
module KindergartenGardenAlternative3

type Plant = Radishes | Clover | Grass | Violets

let charToPlant = function
                  | 'R' -> Plant.Radishes
                  | 'C' -> Plant.Clover
                  | 'G' -> Plant.Grass
                  | 'V' -> Plant.Violets
                  | _ -> failwith "oops"

let plants (diagram:string) (student:string) =
    let studentNum = int student.[0] - int 'A'
    diagram.Split('\n')
        |> List.ofArray
        |> List.map (fun l -> l.Substring(studentNum*2, 2))
        |> List.reduce (+)
        |> List.ofSeq
        |> List.map charToPlant