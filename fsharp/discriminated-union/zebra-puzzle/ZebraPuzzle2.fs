module ZebraPuzzle2

type Owner =    | Norwegian | Japanese  | Englishman | Spaniard    | Ukrainian
type Drink =    | Coffee    | Tea       | Milk       | OrangeJuice | Water
type Color =    | Red       | Ivory     | Green      | Yellow      | Blue
type Animal =   | Zebra     | Dog       | Snail      | Fox         | Horse
type Activity = | Dance     | Paint     | Read       | Football    | Chess

type House = { Owner: Owner; Color: Color; Animal: Animal; Activity: Activity; Drink: Drink; }

let permutations (items: 'a list) : 'a list list =
    let rec insertions x = function
        | [] -> [[x]]
        | (h::t) as l -> (x::l) :: (List.map (fun xs -> h::xs) (insertions x t))
    let rec perm = function
        | [] -> [[]]
        | x::xs -> List.collect (insertions x) (perm xs)
    perm items

let findIndex arr v = Array.findIndex ((=) v) arr
let adjacent i j = abs (i - j) = 1

let owners = [Norwegian; Japanese; Englishman; Spaniard; Ukrainian]
let drinks = [Coffee; Tea; Milk; OrangeJuice; Water]
let colors = [Red; Ivory; Green; Yellow; Blue]
let animals = [Zebra; Dog; Snail; Fox; Horse]
let activities = [Dance; Paint; Read; Football; Chess]

let permsOwners = 
    permutations owners 
    |> List.filter (fun p -> List.head p = Norwegian) 

let permsColors = permutations colors
let permsDrinks = permutations drinks
let permsAnimals = permutations animals
let permsActivities = permutations activities

let solve () =
    seq {
        for nat in permsOwners do
            let natArr = nat |> List.toArray
            let idxEnglish = findIndex natArr Englishman
            let idxSpaniard = findIndex natArr Spaniard
            let idxUkr = findIndex natArr Ukrainian
            let idxNor = findIndex natArr Norwegian
            let idxJapanese = findIndex natArr Japanese

            let colorCandidates =
                permsColors
                |> List.filter (fun col ->
                    let arr = col |> List.toArray
                    let idxGreen = findIndex arr Green
                    let idxIvory = findIndex arr Ivory
                    let idxBlue = findIndex arr Blue
                    idxGreen = idxIvory + 1 &&
                    arr.[idxEnglish] = Red &&
                    adjacent idxNor idxBlue
                )

            for col in colorCandidates do
                let colArr = col |> List.toArray
                let idxGreen = findIndex colArr Green

                let drinkCandidates =
                    permsDrinks
                    |> List.filter (fun dr ->
                        let arr = dr |> List.toArray
                        arr.[2] = Milk &&
                        arr.[idxGreen] = Coffee &&
                        arr.[idxUkr] = Tea
                    )

                for dr in drinkCandidates do
                    let drArr = dr |> List.toArray

                    let animalCandidates =
                        permsAnimals
                        |> List.filter (fun pet ->
                            let arr = pet |> List.toArray
                            arr.[idxSpaniard] = Dog
                        )

                    for pet in animalCandidates do
                        let petArr = pet |> List.toArray

                        let activityCandidates =
                            permsActivities
                            |> List.filter (fun act ->
                                let arr = act |> List.toArray
                                let idxSnail = findIndex petArr Snail
                                let idxYellow = findIndex colArr Yellow
                                let idxFootball = findIndex arr Football
                                let idxReading = findIndex arr Read
                                let idxFox = findIndex petArr Fox
                                let idxPainter = findIndex arr Paint
                                let idxHorse = findIndex petArr Horse
                                arr.[idxSnail] = Dance &&
                                arr.[idxYellow] = Paint &&
                                arr.[idxJapanese] = Chess &&
                                drArr.[idxFootball] = OrangeJuice &&
                                adjacent idxReading idxFox &&
                                adjacent idxPainter idxHorse
                            )

                        for act in activityCandidates do
                            let actArr = act |> List.toArray
                            let houses =
                                [0..4]
                                |> List.map (fun i -> {
                                    Owner = natArr.[i]
                                    Color = colArr.[i]
                                    Drink = drArr.[i]
                                    Animal = petArr.[i]
                                    Activity = actArr.[i]
                                })
                            yield houses
    }

let private getSolution () =
    solve () |> Seq.tryHead

let drinksWater () =
    match getSolution () with
    | Some houses -> houses |> List.find (fun h -> h.Drink = Water) |> fun h -> h.Owner
    | None -> failwith "No solution found."


let ownsZebra () =
    match getSolution () with
    | Some houses -> houses |> List.find (fun h -> h.Animal = Zebra) |> fun h -> h.Owner
    | None -> failwith "No solution found."