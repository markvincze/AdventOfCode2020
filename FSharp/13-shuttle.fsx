open System
open System.IO

let input = File.ReadAllLines "13-shuttle-input.txt"

let earliestTimestamp = input.[0] |> Int32.Parse
let buses = input.[1].Split(',') |> Array.filter (fun i -> i <> "x") |> Array.map Int32.Parse

let busId, waitTime = 
    buses
    |> Array.map (fun i -> i, i - (earliestTimestamp % i))
    |> Array.minBy snd

let result1 = busId * waitTime

// Part 2
let busMap = input.[1].Split(',')
             |> Array.mapi (fun i x -> i, x)
             |> Array.filter (fun (_, x) -> x <> "x")
             |> Array.map (fun (i, x) -> int64 i, x |> Int64.Parse)
             |> Array.sortByDescending snd

let largestBusIndex, largestBusId = busMap |> Array.head 
let matchingIds = busMap
                  |> Array.filter (fun (ix, id) -> ix <> largestBusIndex && (largestBusIndex - ix) % id = 0L)

let rec findSolution2 busMap indexOffset step current =
    if busMap |> Array.forall (fun (ix, id) -> (current - indexOffset + ix) % id = 0L)
    then current - indexOffset
    else findSolution2 busMap indexOffset step (current + step)

let product = largestBusId * (matchingIds |> Array.map snd |> Array.fold ( * ) 1L)

let result2 = findSolution2 busMap largestBusIndex product product
