open System
open System.IO

let numbers = File.ReadAllLines "01-report-input.txt"
              |> Array.map Int32.Parse
              |> List.ofArray

let rec pairs items =
    match items with
    | [] | [_] -> Seq.ofList []
    | [ a; b ] -> Seq.ofList [(a, b)]
    | h :: t -> seq {
                    for x in t do
                        yield (h, x)
                    yield! pairs t
                }

let a1, b1 =
    numbers
    |> pairs
    |> Seq.find (fun (a,b) -> a + b = 2020)

let result1 = a1 * b1

let rec triplets items =
    match items with
    | [] | [_] | [_;_] -> Seq.ofList []
    | [ a; b; c ] -> Seq.ofList [(a, b, c)]
    | h :: t -> let ts = triplets t
                let ps = pairs t
                seq {
                    for (a, b) in ps do 
                        yield (a, b, h)
                    yield! ts
                }

let a2, b2, c2 =
    numbers
    |> triplets
    |> Seq.find (fun (a, b, c) -> a + b + c = 2020)

let result2 = a2 * b2 * c2