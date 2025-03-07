module KindergartenGarden

open System

// TODO: define the Plant type
type Plant = | Grass = 'G' | Clover = 'C' | Radishes = 'R' | Violets = 'V'
type Student = | Alice = 0 | Bob = 2 | Charlie = 4 | David = 6 | Eve = 8 | Fred = 10 | Ginny = 12 | Harriet = 14 | Ileana = 16 | Joseph = 18 | Kincaid = 20 | Larry = 22

let asPlant (c:char) = 
    match c with
    | 'G' -> Plant.Grass
    | 'C' -> Plant.Clover
    | 'R' -> Plant.Radishes
    | 'V' -> Plant.Violets
    | _ -> failwith "Unknown plant"

let asStudent (name:string) =
    match name with
    | "Alice" -> Student.Alice
    | "Bob" -> Student.Bob
    | "Charlie" -> Student.Charlie
    | "David" -> Student.David
    | "Eve" -> Student.Eve
    | "Fred" -> Student.Fred
    | "Ginny" -> Student.Ginny
    | "Harriet" -> Student.Harriet
    | "Ileana" -> Student.Ileana
    | "Joseph" -> Student.Joseph
    | "Kincaid" -> Student.Kincaid
    | "Larry" -> Student.Larry
    | _ -> failwith "Unknown student"

let plants (diagram: string) (student: string): Plant list = 
    let plantRows = diagram.Split "\n"
    
    let st = asStudent student

    let firstRow = plantRows[0].Substring (int st, 2)
    let secondRow = plantRows[1].Substring (int st, 2)

    firstRow + secondRow
    |> Seq.map asPlant 
    |> Seq.toList
