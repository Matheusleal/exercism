module Meetup

open System

type Week = First | Second | Third | Fourth | Last | Teenth

let meetup (year: int) (month: int) (week: Week) (dayOfWeek: DayOfWeek) : DateTime =
    let daysInMonth = DateTime.DaysInMonth(year, month)

    let findMatchingDate startDay endDay =
        let startDate = DateTime(year, month, startDay)
        let offset = (int dayOfWeek - int startDate.DayOfWeek + 7) % 7
        let resultDay = startDay + offset

        if resultDay <= endDay 
            then DateTime(year, month, resultDay)
            else failwith "No matching day found in range"

    match week with
    | Teenth -> findMatchingDate 13 19
    | First  -> findMatchingDate 1 7
    | Second -> findMatchingDate 8 14
    | Third  -> findMatchingDate 15 21
    | Fourth -> findMatchingDate 22 28
    | Last   -> findMatchingDate (daysInMonth - 6) daysInMonth
