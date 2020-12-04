open System
open System.IO

type Square =
| Open
| Tree

let parseSquare c = match c with
                    | '.' -> Open
                    | '#' -> Tree
                    | _ -> failwith "Invalid input"

let lines = File.ReadAllLines "03-trajectory-input.txt"
            |> Array.map (fun l -> Array.ofSeq l)

let grid =
    Array2D.init
        (Array.length lines.[0])
        (Array.length lines)
        (fun x y -> lines.[y].[x] |> parseSquare)

let square (grid : Square[,]) (x, y) =
    grid.[x % (Array2D.length1 grid), y % (Array2D.length2 grid)]

let rec countTrees grid (x, y) (sx, sy) cnt =
    if y >= Array2D.length2 grid
    then cnt |> int64
    else if square grid (x, y) = Tree
         then countTrees grid (x + sx, y + sy) (sx, sy) (cnt + 1)
         else countTrees grid (x + sx, y + sy) (sx, sy) cnt

let result1 = countTrees grid (0, 0) (3, 1) 0

let result2 = 
    (countTrees grid (0, 0) (1, 1) 0) *
    (countTrees grid (0, 0) (3, 1) 0) *
    (countTrees grid (0, 0) (5, 1) 0) *
    (countTrees grid (0, 0) (7, 1) 0) *
    (countTrees grid (0, 0) (1, 2) 0)