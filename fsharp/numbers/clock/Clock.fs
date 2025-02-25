module Clock

type Clock = {
    Hours : int
    Minutes : int
}

let create hours minutes =
    let rec normalize (h : int) (m : int) =
        match h, m with
        | h, m when m >= 60 -> normalize (h+1) (m-60)
        | h, m when m < 0 -> normalize (h-1) (m+60)
        | h, m when h >= 24 -> normalize (h-24) m
        | h, m when h < 0 -> normalize (h+24) m
        | _ -> {Hours = h; Minutes = m}

    normalize hours minutes

let add (minutes : int) (clock : Clock) = create clock.Hours (clock.Minutes + minutes)

let subtract (minutes : int) (clock : Clock) = create clock.Hours (clock.Minutes - minutes)

let display (clock : Clock) = sprintf "%02d:%02d" clock.Hours clock.Minutes
