open System
open System.IO

type Direction =
| East
| Southeast
| Southwest
| West
| Northwest
| Northeast

let parseLine line =
    let rec parseLine line steps index =
        if index >= String.length line
        then steps |> List.rev
        else match line.[index] with
             | 'e' -> parseLine line (East :: steps) (index + 1)
             | 'w' -> parseLine line (West :: steps) (index + 1)
             | _ -> match line.[index], line.[index + 1] with
                    | 's', 'e' -> parseLine line (Southeast :: steps) (index + 2)
                    | 's', 'w' -> parseLine line (Southwest :: steps) (index + 2)
                    | 'n', 'e' -> parseLine line (Northeast :: steps) (index + 2)
                    | 'n', 'w' -> parseLine line (Northwest :: steps) (index + 2)
                    | _ -> failwith "Invalid input"
    parseLine line [] 0

let stepList = File.ReadAllLines "24-lobby-layout-input.txt"
               |> Array.map parseLine
               |> List.ofArray

let step d (q, r) = match d with
                    | East -> q + 1, r
                    | Southeast -> q, r + 1
                    | Southwest -> q - 1, r + 1
                    | West -> q - 1, r
                    | Northwest -> q, r - 1
                    | Northeast -> q + 1, r - 1

let endPosition steps =
    steps |> List.fold (fun pos dir -> step dir pos) (0, 0)

let flips = stepList
            |> List.map endPosition
            |> List.fold
                (fun flips pos -> flips |> Map.change pos (Option.defaultValue false >> not >> Some))
                Map.empty

let result1 = flips 
              |> Seq.filter (fun kvp -> kvp.Value % 2 = 1)
              |> Seq.length

let neighbors (q, r) = [(q + 1, r); (q, r + 1); (q - 1, r + 1); (q - 1, r); (q, r - 1); (q + 1, r - 1)]

let expand flips
