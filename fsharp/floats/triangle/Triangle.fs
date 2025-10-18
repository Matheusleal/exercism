module Triangle

let equilateral (triangle: float list): bool = 
    List.sort triangle
    |> function
        | [a; _; _] when a <= 0.0 -> false
        | [a; b; c] -> a = b && b = c
        | _ -> false

let isosceles (triangle: float list) =
    List.sort triangle
    |> function
        | [a; _; _] when a <= 0.0 -> false
        | [a; b; c] when a = b || b = c -> a + b >= c
        | _ -> false 

let scalene (triangle: float list) =
    List.sort triangle
    |> function
        | [a; _; _] when a <= 0.0 -> false
        | [a; b; c] when a + b > c -> a <> b && b <> c && a <> c
        | _ -> false