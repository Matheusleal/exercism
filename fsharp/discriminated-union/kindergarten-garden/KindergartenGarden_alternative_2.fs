// https://exercism.org/tracks/fsharp/exercises/kindergarten-garden/solutions/steffengodskesen
module KindergartenGardenAlternative2

type Plant = Grass | Clover | Radishes | Violets

let private toPlant pot =
    match pot with
    | 'G' -> Grass
    | 'C' -> Clover
    | 'R' -> Radishes
    | 'V' -> Violets
    | _ -> failwith "Unknown plant"

let private students =
    ["Alice"; "Bob"; "Charlie"; "David";
     "Eve"; "Fred"; "Ginny"; "Harriet";
     "Ileana"; "Joseph"; "Kincaid"; "Larry"]

let plants diagram student =
    let i = List.findIndex (fun s -> s = student) students
    diagram
    |> fun (s:string) -> s.Split "\n"
    |> Seq.collect (fun row -> row |> Seq.skip (2*i) |> Seq.take 2)
    |> Seq.map toPlant
    |> Seq.toList