open System
open System.IO

let numbers = File.ReadAllLines "01-report-input.txt"
              |> Array.map Int32.Parse
              |> List.ofArray

let rec pairs items =
    match items with
    | [] -> []
    | [ a ] -> []
    | [ a; b ] -> [ (a, b) ]
    | h :: t -> let ps1 = pairs t
                let ps2 = t |> List.map (fun x -> (h, x))
                List.concat [ ps1; ps2 ]

let a1, b1 =
    numbers
    |> pairs
    |> List.find (fun (a,b) -> a + b = 2020)

let result1 = a1 * b1

let rec triplets items =
    let itemsA = Array.ofList items
    let l = Array.length itemsA
    match items with
    | [] -> []
    | [ a ] -> []
    | [ a; b ] -> []
    | _ -> [ for i in 0..(l - 1) do
               for j in 0..(l - 1) do
                   for k in 0..(l - 1) do
                       if i = j || i = k || j = k
                       then ()
                       else yield (itemsA.[i], itemsA.[j], itemsA.[k]) ]

let a2, b2, c2 =
    numbers
    |> triplets
    |> List.find (fun (a, b, c) -> a + b + c = 2020)

let result2 = a2 * b2 * c2