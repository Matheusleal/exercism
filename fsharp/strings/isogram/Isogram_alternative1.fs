// https://exercism.org/tracks/fsharp/exercises/isogram/solutions/marionebl
module IsogramAlternative1

open System

let isIsogram (str: string): bool =  
    str
    |> Seq.filter Char.IsLetter
    |> Seq.countBy Char.ToLower
    |> Seq.forall (fun (_, c) -> c = 1)
    // |> Seq.forall (fun count -> snd count = 1)