module CollatzConjecture

let steps (number: int): int option = 
    
    let isOdd (n : int): bool = n % 2 = 1
    let oddCalc (n : int): int = 3 * n + 1
    let evenCalc (n : int): int = n / 2
    let applyCalc (n: int, c : int) =
        let r = if isOdd n then oddCalc n else evenCalc n
        r, c + 1

    let rec exec (n: int, i: int) =
        match n with
        | 0 -> None
        | 1 -> Some i
        | v when v <= 0 -> None
        | v -> applyCalc (v, i) |> exec 

    exec (number, 0)
    

let steps2 (number: int): int option =
    
    let (|Odd|Even|) (n: int) = if n % 2 = 1 then Odd else Even
    let oddCalc (n : int): int = 3 * n + 1
    let evenCalc (n : int): int = n / 2
    
    let collatzCounter (n: int) =
        
        let rec loop (n: int, c: int) =
            match n with
            | 1 -> Some c
            | Odd -> loop (oddCalc n, c + 1)
            | Even -> loop (evenCalc n, c + 1)
        
        loop (n,0)

    if number <= 0 
        then None 
        else collatzCounter number
