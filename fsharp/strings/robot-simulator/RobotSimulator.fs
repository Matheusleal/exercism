module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { Direction: Direction; Position: Position}

let create (direction: Direction) (position: Position): Robot = {Direction = direction; Position = position}

let move (instructions: string) (robot: Robot): Robot =   
    let rec go (m: char list) (r: Robot) =
        match m with 
        | 'R'::t ->
                let newDirection = 
                    match r.Direction with
                        | North -> East
                        | East -> South
                        | South -> West
                        | West -> North
                go t {r with Direction = newDirection}
        
        | 'L'::t -> 
                let newDirection =
                    match r.Direction with
                        | North -> West
                        | East -> North
                        | South -> East
                        | West -> South
                go t {r with Direction = newDirection}
        
        | 'A'::t -> 
                match r.Direction with
                    | North -> go t {r with Position = fst r.Position, snd r.Position + 1}
                    | East -> go t {r with Position = fst r.Position + 1, snd r.Position}
                    | South -> go t {r with Position = fst r.Position , snd r.Position - 1}
                    | West -> go t {r with Position = fst r.Position - 1, snd r.Position}
        | _ -> r

    go (List.ofSeq instructions) robot