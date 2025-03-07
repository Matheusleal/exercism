module Isogram

let isIsogram (str: string): bool =  
    str.ToLowerInvariant()
    |> Seq.filter (fun x -> x <> ' ' && x <> '-')
    |> (fun x -> (set x).Count - Seq.length x) = 0