module Anagram

let sort (value: string) = value |> Seq.sortBy id |> Seq.toList
let (|ToLower|) (value: string) = value.ToLower()
let findAnagrams (sources: seq<string>) (ToLower target : string) =
    sources
        |> Seq.filter (fun ( ToLower source) -> sort source = sort target && source <> target)
        |> Seq.toList
