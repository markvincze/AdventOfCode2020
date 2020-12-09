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
