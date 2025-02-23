module Allergies

open System

type Allergen =
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128

let allergiesList =  
    Enum.GetValues typeof<Allergen> |> Seq.cast |> Seq.toList |> List.rev

let allergicTo (codedAllergies: int) (allergen: Allergen) = 
    codedAllergies &&& int allergen <> 0

let list codedAllergies = 

    let rec loopWise(codedAllergies: int, list: Allergen list) =
        match codedAllergies, list with
        | 0, _ -> []
        | n, h::t -> 
            if allergicTo n h 
            then h :: loopWise(n, t) 
            else loopWise(n, t)
        | _ -> []

    loopWise(codedAllergies, allergiesList)
    |> List.rev