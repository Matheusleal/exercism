module AnnalynsInfiltration

let canFastAttack
    (knightIsAwake: bool): bool =
        not knightIsAwake


let canSpy
    (knightIsAwake: bool)
    (archerIsAwake: bool)
    (prisonerIsAwake: bool): bool =
        knightIsAwake ||
        archerIsAwake ||
        prisonerIsAwake

let canSignalPrisoner
    (archerIsAwake: bool)
    (prisonerIsAwake: bool): bool =
        not archerIsAwake &&
        prisonerIsAwake

let canFreePrisoner
    (knightIsAwake: bool)
    (archerIsAwake: bool)
    (prisonerIsAwake: bool)
    (petDogIsPresent: bool): bool =
        (not archerIsAwake && petDogIsPresent) ||
        (not knightIsAwake && not archerIsAwake && prisonerIsAwake)
