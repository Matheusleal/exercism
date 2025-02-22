module Leap

let leapYear (year: int): bool =
    if year % 100 = 0
        then year % 400 = 0
        else year % 4 = 0