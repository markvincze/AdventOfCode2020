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

let tiles = stepList
            |> List.map endPosition
            |> List.fold
                (fun flips pos -> match Map.tryFind pos flips with
                                  | None -> Map.add pos true flips
                                  | Some isBlack -> Map.add pos (not isBlack) flips)
                Map.empty

let blackTileCount (tiles : Map<(int * int), bool>) =
    tiles 
    |> Seq.filter (fun kvp -> kvp.Value)
    |> Seq.length

let result1 = blackTileCount tiles 

let neighbors (q, r) = [(q + 1, r); (q, r + 1); (q - 1, r + 1); (q - 1, r); (q, r - 1); (q + 1, r - 1)]

let initIfMissing tiles pos = match Map.tryFind pos tiles with
                              | None -> Map.add pos false tiles
                              | Some _ -> tiles

let expand tiles =
    tiles
    |> Map.filter (fun _ isBlack -> isBlack)
    |> Map.fold
        (fun expanded pos color ->
            neighbors pos
            |> List.fold initIfMissing expanded)
        tiles

let blackNeighborCount tiles pos =
    neighbors pos
    |> List.filter (fun p -> Map.tryFind p tiles |> Option.defaultValue false)
    |> List.length

let evolve tiles =
    let tiles = expand tiles
    tiles
    |> Map.map (fun pos isBlack -> match isBlack, blackNeighborCount tiles pos with
                                   | true, 0 -> false
                                   | true, bnc when bnc > 2 -> false
                                   | false, 2 -> true
                                   | isBlack, _ -> isBlack)

let rec evolveN tiles n =
    match n with
    | 0 -> tiles
    | n -> evolveN (evolve tiles) (n - 1)

let final = evolveN tiles 100
let result2 = blackTileCount final