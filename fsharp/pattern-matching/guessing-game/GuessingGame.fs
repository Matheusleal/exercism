module GuessingGame

let reply (guess: int): string = 
    let correctNumber = 42 // I don't like magic numbers

    match guess with
    | x when x = correctNumber -> "Correct"
    | x when x + 1 = correctNumber 
        || x - 1 = correctNumber -> "So close"
    | x when x < correctNumber -> "Too low"
    | x when x > correctNumber -> "Too high"
    | _ -> "How did you get here?"

