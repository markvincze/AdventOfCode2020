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

let rec countTrees grid (x, y) cnt =
    if y >= Array2D.length2 grid
    then cnt
    else if square grid (x, y) = Tree
         then countTrees grid (x + 3, y + 1) (cnt + 1)
         else countTrees grid (x + 3, y + 1) cnt

let result1 = countTrees grid (0, 0) 0