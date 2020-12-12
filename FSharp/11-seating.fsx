open System
open System.IO

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
