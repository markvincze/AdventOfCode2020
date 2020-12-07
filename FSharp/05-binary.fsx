open System
open System.IO

let lines = File.ReadAllLines "05-binary-input.txt"

let row (pass : string) =
    [ 0..6 ]
    |> List.sumBy (fun i -> if pass.[i] = 'B' then (Math.Pow(2.0, (6 - i) |> float)) |> int else 0)

let column (pass : string) =
    [ 7..9 ]
    |> List.sumBy (fun i -> if pass.[i] = 'R' then (Math.Pow(2.0, (9 - i) |> float)) |> int else 0)

let seatId pass = (row pass) * 8 + (column pass)

let result1 = lines
              |> Array.map seatId
              |> Array.max

let minId = lines |> Array.map seatId |> Array.min
let maxId = lines |> Array.map seatId |> Array.max
let allSeats = lines |> Array.map seatId |> Set.ofArray

let result2 = [ minId..maxId ]
              |> List.find (fun i -> (not (Set.contains i allSeats)) && (Set.contains (i - 1) allSeats) && (Set.contains (i + 1) allSeats))
