open System
open System.IO

type Direction =
| East
| South
| West
| North

let parse (line : string) =
    (line.[0], line.Substring 1 |> Int32.Parse)

let commands = File.ReadAllLines "12-rain-risk-input.txt"
               |> List.ofArray
               |> List.map parse

let changeDir degrees dir =
    let dirs = [| East; South; West; North |]
    let di = dirs |> Array.findIndex (fun d -> d = dir)
    let newIndex = match (di + (degrees / 90)) with
                   | ni when ni >= 0 -> ni % 4
                   | ni when ni < 0 -> 4 + ni
    dirs.[newIndex]

let move dir dist (x, y) =
    match dir with
    | East -> x + dist, y
    | South -> x, y - dist
    | West -> x - dist, y
    | North -> x, y + dist

let doAction (action, value) pos dir =
    match action with
    | 'F' -> move dir value pos, dir
    | 'E' -> move East value pos, dir
    | 'S' -> move South value pos, dir
    | 'W' -> move West value pos, dir
    | 'N' -> move North value pos, dir
    | 'R' -> pos, changeDir value dir
    | 'L' -> pos, changeDir -value dir
    | _ -> failwith "Invalid action"

let rec processCommands commands pos dir =
    match commands with
    | [] -> pos
    | h :: t -> let pos, dir = doAction h pos dir
                processCommands t pos dir

let x, y = processCommands commands (0, 0) East
let result1 = abs(x) + abs(y)

// Part 2
let rotate degrees (x, y) =
    match degrees with
    | 0 -> (x, y)
    | 90 | -270 -> (y, -x)
    | 180 | -180 -> (-x, -y)
    | 270 | -90 -> (-y, x)
    | _ -> failwith "Invalid rotation degrees"

let moveToPos (toX, toY) times (fromX, fromY) =
    (fromX + times * toX, fromY + times * toY)

let doActionWp (action, value) shipPos wpPos =
    match action with
    | 'F' -> moveToPos wpPos value shipPos, wpPos
    | 'E' -> shipPos, move East value wpPos
    | 'S' -> shipPos, move South value wpPos
    | 'W' -> shipPos, move West value wpPos
    | 'N' -> shipPos, move North value wpPos
    | 'R' -> shipPos, rotate value wpPos
    | 'L' -> shipPos, rotate -value wpPos
    | _ -> failwith "Invalid action"

let rec processCommandsWp commands shipPos wpPos =
    match commands with
    | [] -> shipPos
    | h :: t -> let shipPos, wpPos = doActionWp h shipPos wpPos
                processCommandsWp t shipPos wpPos

let x2, y2 = processCommandsWp commands (0, 0) (10, 1)
let result2 = abs(x2) + abs(y2)
