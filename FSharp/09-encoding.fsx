open System
open System.IO

let input = File.ReadAllLines "09-encoding-input.txt"
            |> Array.map Int64.Parse

let isThereSumPair (input : int64 array) from until value =
    let pairs = seq {
        for i in from..until do
            for j in (i + 1)..until do
                if i <> j
                then yield (input.[i], input.[j])
    }

    pairs
    |> Seq.exists (fun (a, b) -> a <> b && a + b = value)

let rec findFirstInvalidNumber (input : int64 array) preamble index =
    if not (isThereSumPair input (index - preamble) (index - 1) input.[index])
    then input.[index]
    else findFirstInvalidNumber input preamble (index + 1)

let result1 = findFirstInvalidNumber input 25 25

let tryFindMatchingSet input value start =
    let rec tryFindMatchingSetFrom (input : int64 array) value start currentEnd acc =
        if currentEnd >= Array.length input
        then None
        else if acc + input.[currentEnd] = value
             then Some (start, currentEnd)
             else if acc + input.[currentEnd] > value
             then None
             else tryFindMatchingSetFrom input value start (currentEnd + 1) (acc + input.[currentEnd])

    tryFindMatchingSetFrom input value start (start + 1) (input.[start])

let findMatchingSet input value =
    seq { for start in 0..(Array.length input) - 2 do
            yield tryFindMatchingSet input value start }
    |> Seq.pick id

let first, last = findMatchingSet input result1
let region = input.[first..last]

let result2 = (Array.min region) + (Array.max region)
