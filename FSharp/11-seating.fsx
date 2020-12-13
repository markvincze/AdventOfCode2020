open System
open System.IO

type Direction =
| Left
| UpLeft
| Up
| UpRight
| Right
| DownRight
| Down
| DownLeft

let allDirections = [ Left; UpLeft; Up; UpRight; Right; DownRight; Down; DownLeft ]

type Position =
| Floor
| EmptySeat
| Occupied

let parse ch = match ch with
               | 'L' -> EmptySeat
               | '.' -> Floor
               | _ -> failwith "Invalid input"

let lines = File.ReadAllLines "11-seating-input.txt"
let grid = Array2D.init
               (String.length lines.[0])
               (Array.length lines)
               (fun x y -> lines.[y].[x] |> parse)

let adjacents (x, y) (grid : Position[,])  =
    seq {
        if x > 0 then yield grid.[x - 1, y]
        if x > 0 && y > 0 then yield grid.[x - 1, y - 1]
        if y > 0 then yield grid.[x, y - 1]
        if x < (Array2D.length1 grid) - 1 && y > 0 then yield grid.[x + 1, y - 1]
        if x < (Array2D.length1 grid) - 1 then yield grid.[x + 1, y]
        if x < (Array2D.length1 grid) - 1 && y < (Array2D.length2 grid) - 1 then yield grid.[x + 1, y + 1]
        if y < (Array2D.length2 grid) - 1 then yield grid.[x, y + 1]
        if x > 0 && y < (Array2D.length2 grid) - 1 then yield grid.[x - 1, y + 1]
    }

let evolve (grid : Position[,]) =
    Array2D.init
        (Array2D.length1 grid)
        (Array2D.length2 grid)
        (fun x y ->
            if grid.[x, y] = EmptySeat && adjacents (x, y) grid |> Seq.forall (fun p -> p <> Occupied)
            then Occupied
            else if grid.[x, y] = Occupied && (adjacents (x, y) grid |> Seq.filter (fun p -> p = Occupied) |> Seq.length) >= 4
            then EmptySeat
            else grid.[x, y])

let rec countEvolutions grid acc =
    let newGrid = evolve grid
    if newGrid = grid
    then newGrid |> Seq.cast<Position> |> Seq.filter (fun p -> p = Occupied) |> Seq.length
    else countEvolutions newGrid (acc + 1)

let result1 = countEvolutions grid 0

let step dir (x, y) =
    match dir with 
    | Left -> (x - 1, y)
    | UpLeft -> (x - 1, y - 1)
    | Up -> (x, y - 1)
    | UpRight -> (x + 1, y - 1)
    | Right -> (x + 1, y)
    | DownRight -> (x + 1, y + 1)
    | Down -> (x, y + 1)
    | DownLeft -> (x - 1, y + 1)

let at (x, y) grid =
    if x >= 0 && x < (Array2D.length1 grid) && y >= 0 && y < (Array2D.length2 grid)
    then Some grid.[x, y]
    else None

let firstVisibleSeat from (grid : Position[,]) dir  =
    Seq.unfold
        (fun pos -> match at (step dir pos) grid with
                    | Some p -> Some (p, step dir pos)
                    | None -> None)
        from
    |> Seq.tryPick (fun p -> match p with
                             | EmptySeat -> Some EmptySeat
                             | Occupied -> Some Occupied
                             | _ -> None)
    |> Option.defaultValue EmptySeat

let evolve2 (grid : Position[,]) =
    Array2D.init
        (Array2D.length1 grid)
        (Array2D.length2 grid)
        (fun x y ->
            if grid.[x, y] = EmptySeat &&
               allDirections |> List.map (firstVisibleSeat (x, y) grid) |> List.forall (fun p -> p <> Occupied)
            then Occupied
            else if grid.[x, y] = Occupied &&
                    (allDirections |> List.map (firstVisibleSeat (x, y) grid) |> List.filter (fun p -> p = Occupied)) |> List.length >= 5
            then EmptySeat
            else grid.[x, y])

let rec countEvolutions2 grid acc =
    let newGrid = evolve2 grid
    if newGrid = grid
    then newGrid |> Seq.cast<Position> |> Seq.filter (fun p -> p = Occupied) |> Seq.length
    else countEvolutions2 newGrid (acc + 1)

let result2 = countEvolutions2 grid 0
