module Meetup2

open System

type Week = First | Second | Third | Fourth | Last | Teenth

let meetup (year: int) (month: int) (week: Week) (dayOfWeek: DayOfWeek) : DateTime =
    let daysInMonth = DateTime.DaysInMonth(year, month)

    let dateFromRange startDay endDay =
        seq { startDay .. endDay }
        |> Seq.map (fun d -> DateTime(year, month, d))
        |> Seq.find (fun dt -> dt.DayOfWeek = dayOfWeek)

    match week with
    | Teenth -> dateFromRange 13 19
    | First  -> dateFromRange 1 7
    | Second -> dateFromRange 8 14
    | Third  -> dateFromRange 15 21
    | Fourth -> dateFromRange 22 28
    | Last   -> dateFromRange (daysInMonth - 6) daysInMonth
