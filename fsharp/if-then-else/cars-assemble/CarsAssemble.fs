module CarsAssemble

let successRate (speed: int): float =
    if speed = 0 then 0.0
    elif speed < 5 then 1.0
    elif speed < 9 then 0.9
    elif speed < 10 then 0.8
    else 0.77

let productionRatePerHour (speed: int): float =
    let productionRatePerSpeed = 221.0
    let currentProduction = float(speed) * productionRatePerSpeed
    currentProduction * successRate speed


let workingItemsPerMinute (speed: int): int =
    productionRatePerHour speed
    |> (fun success -> success / 60.0)
    |> int