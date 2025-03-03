// After been mentored bt neuroevolutus in exercism he geves me some tips to improve my code

module YachtV2

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

let score (category: Category) (dice: Die list): int = 
    
    let filterDie (dice: Die list) (die: Die): Die list = List.filter ((=) die) dice
    let sumDice (dice: Die list): int = List.sumBy int dice
    let calcSingles (dice: Die list) (die: Die): int =  List.length (filterDie dice die) * int die
    let calcFullHouse dice = 
        let group = List.groupBy id dice |> List.sortByDescending (snd >> List.length)

        if group |> List.length = 2
            && group.Head |> fun (die, dice) -> List.length dice = 3
            && group.Tail.Head |> fun (die, dice) -> List.length dice = 2
        then sumDice dice 
        else 0
    
    let calcFourForAKing dice =
        List.groupBy id dice 
        |> List.sortByDescending fst
        |> List.head
        |> fun (die, dice) ->  
            if List.length dice >= 4 
            then List.take 4 dice |> sumDice 
            else 0

    match category with
    | Ones -> calcSingles dice Die.One
    | Twos -> calcSingles dice Die.Two
    | Threes -> calcSingles dice Die.Three
    | Fours -> calcSingles dice Die.Four
    | Fives -> calcSingles dice Die.Five
    | Sixes -> calcSingles dice Die.Six
    | FullHouse -> calcFullHouse dice
    | FourOfAKind -> calcFourForAKing dice
    | LittleStraight -> if List.sort dice = [Die.One; Die.Two; Die.Three; Die.Four; Die.Five] then 30 else 0
    | BigStraight -> if List.sort dice = [Die.Two; Die.Three; Die.Four; Die.Five; Die.Six] then 30 else 0
    | Choice -> sumDice dice
    | Yacht -> if Set.ofList dice |> Set.count = 1 then 50 else 0