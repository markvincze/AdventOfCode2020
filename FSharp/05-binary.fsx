open System
open System.IO

let lines = File.ReadAllLines "05-binary-input.txt"

let row (pass : string) =
    (if pass.[0] = 'B' then 64 else 0) +
    (if pass.[1] = 'B' then 32 else 0) +
    (if pass.[2] = 'B' then 16 else 0) +
    (if pass.[3] = 'B' then 8 else 0) +
    (if pass.[4] = 'B' then 4 else 0) +
    (if pass.[5] = 'B' then 2 else 0) +
    (if pass.[6] = 'B' then 1 else 0)

let column (pass : string) =
    (if pass.[7] = 'R' then 4 else 0) +
    (if pass.[8] = 'R' then 2 else 0) +
    (if pass.[9] = 'R' then 1 else 0)

let seatId pass = (row pass) * 8 + (column pass)

let result1 = lines
              |> Array.map seatId
              |> Array.max

let minId = lines |> Array.map seatId |> Array.min
let maxId = lines |> Array.map seatId |> Array.max
let allSeats = lines |> Array.map seatId |> Set.ofArray

let result2 = [ minId..maxId ]
              |> List.find (fun i -> (not (Set.contains i allSeats)) && (Set.contains (i - 1) allSeats) && (Set.contains (i + 1) allSeats))
