module BookingUpForBeauty

// The following line is needed to use the DateTime type
open System

let schedule (appointmentDateDescription: string): DateTime =
    match System.DateTime.TryParse appointmentDateDescription with
    | true, date -> date
    | _ -> failwith "Invalid appointment date"

let hasPassed (appointmentDate: DateTime): bool =
    DateTime.Now > appointmentDate

let isAfternoonAppointment (appointmentDate: DateTime): bool =
    match appointmentDate.Hour with
    | x when x >= 12 && x < 18 -> true
    | _ -> false

let description (appointmentDate: DateTime): string =
    sprintf "You have an appointment on %s." (appointmentDate.ToString("M/d/yyyy h:mm:ss tt"))

let anniversaryDate(): DateTime =
    DateTime(DateTime.Now.Year, 9, 15)
