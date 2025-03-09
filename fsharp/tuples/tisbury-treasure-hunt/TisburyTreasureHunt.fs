module TisburyTreasureHunt

let getCoordinate (line: string * string): string = snd line

let convertCoordinate (coordinate: string): int * char =  int coordinate.[0..0], coordinate.[1]

let compareRecords (azarasData: string * string) ((_, coordinate, _): string * (int * char) * string) : bool = 
    convertCoordinate (snd azarasData) = coordinate

let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =
    let treasure, coordinate = azarasData
    let location, _, quadrant = ruisData

    if compareRecords azarasData ruisData
    then coordinate, location, quadrant, treasure
    else "","","",""