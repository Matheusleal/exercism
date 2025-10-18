module BinarySearch

let find (input: int array) (value: int): int option =
    let rec binarySearch (low: int) (mid: int) (high: int) =
        match low, mid, high with
        | l, _, _ when value = input[l] -> Some l
        | _, m, _ when value = input[m] -> Some m
        | _, _, h when value = input[h] -> Some h
        | l, m, h when l > m -> binarySearch l (l+1) h
        | l, m, h when m > h -> binarySearch l (h-1) h
        | l, m, h when h - l = 3 ->
                                if input[m-1] = value then Some (m-1)
                                elif input[m+1] = value then Some (m+1)
                                else None
        | l, m, _ when value < input[m] -> binarySearch l (l+(m/4)) m
        | _, m, h when value > input[m] -> binarySearch m (m+(h/4)) h
        | _, _, _ -> None
    
    let items = input.Length
    let itemsIndex = (items-1)

    if items = 0 then None
    elif value < input[0] || value > input[itemsIndex] then None
    elif items = 1 && input[0] = value then Some 0
    else binarySearch 0 ((itemsIndex)/2) itemsIndex
