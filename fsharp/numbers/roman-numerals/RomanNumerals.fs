module RomanNumerals

type RomanNumeral = 
    | I = 1
    | V = 5
    | X = 10
    | L = 50
    | C = 100
    | D = 500
    | M = 1000

let roman arabicNumeral = 

    let rec convert arabicNumeral = 
        match arabicNumeral with
        | 0 -> ""
        | n when n >= 1000 -> "M" + convert (n - 1000)
        | n when n >= 900 -> "CM" + convert (n - 900)
        | n when n >= 500 -> "D" + convert (n - 500)
        | n when n >= 400 -> "CD" + convert (n - 400)
        | n when n >= 100 -> "C" + convert (n - 100)
        | n when n >= 90 -> "XC" + convert (n - 90)
        | n when n >= 50 -> "L" + convert (n - 50)
        | n when n >= 40 -> "XL" + convert (n - 40)
        | n when n >= 10 -> "X" + convert (n - 10)
        | n when n >= 9 -> "IX" + convert (n - 9)
        | n when n >= 5 -> "V" + convert (n - 5)
        | n when n >= 4 -> "IV" + convert (n - 4)
        | n when n > 0 -> "I" + convert (n - 1)
        | _ -> ""

    convert arabicNumeral
        