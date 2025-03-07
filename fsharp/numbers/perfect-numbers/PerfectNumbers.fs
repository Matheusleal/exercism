module PerfectNumbers

type Classification = Perfect | Abundant | Deficient 

let classify (n: int) : Classification option = 
    if n = 1 
        then Some Deficient
    else
        [1.. n/2]
        |> List.filter (fun x -> n % x = 0)
        |> List.sum
        |> function
            | x when x < 1 -> None
            | x when x < n -> Some Deficient
            | x when x = n -> Some Perfect
            | _ -> Some Abundant