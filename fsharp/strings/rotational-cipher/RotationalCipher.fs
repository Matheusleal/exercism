module RotationalCipher

open System

let rotate (shiftKey: int) (text: string) =  

    let spin (current: char) (toAdd: int) (fstChar: char, lstChar: char) =         
        let curr, fst, lst = int current, int fstChar, int lstChar

        if curr + toAdd > lst
        then char (curr + toAdd - lst + fst - 1)
        else char (curr + toAdd)

    let rotor (times: int ) (letter: char) =
        match letter, Char.IsLetter letter, Char.IsUpper letter with
        | l, true, true -> spin l times ('A','Z')
        | l, true, false -> spin l times ('a','z')
        | _ -> letter
    
    text |> String.map (rotor shiftKey)
    