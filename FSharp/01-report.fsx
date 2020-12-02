open System
open System.IO

let numbers = File.ReadAllLines "01-report-input.txt"
              |> Array.map Int32.Parse
              |> List.ofArray

let rec pairs items =
    match items with
    | [] | [_] -> []
    | [ a; b ] -> [(a, b)]
    | h :: t -> let ps1 = pairs t
                let ps2 = t |> List.map (fun x -> (h, x))
                List.concat [ ps1; ps2 ]

let a1, b1 =
    numbers
    |> pairs
    |> List.find (fun (a,b) -> a + b = 2020)

let result1 = a1 * b1

let rec triplets items =
    match items with
    | [] | [_] | [_;_] -> []
    | [ a; b; c ] -> [(a, b, c)]
    | h :: t -> let ts = triplets t
                let ps = pairs t
                [ for (a, b) in ps do 
                    yield (a, b, h)
                  yield! ts ]

let a2, b2, c2 =
    numbers
    |> triplets
    |> List.find (fun (a, b, c) -> a + b + c = 2020)

let result2 = a2 * b2 * c2