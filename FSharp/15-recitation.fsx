let input = [0;20;7;16;1;18;15]

let cache = input |> List.take ((List.length input) - 1) |> List.mapi (fun i n -> (n, (i + 1))) |> Map.ofList

let play cache round previousNumber =
    match Map.tryFind previousNumber cache with
    | None -> cache |> Map.add previousNumber (round - 1), 0
    | Some r -> cache |> Map.add previousNumber (round - 1), round - 1 - r

let rec playUntil until cache round previousNumber =
    let cache, newNumber = play cache round previousNumber

    if round = until
    then newNumber
    else playUntil until cache (round + 1) newNumber

let result1 = playUntil 2020 cache ((input |> List.length) + 1) (input |> List.last) 

let result2 = playUntil 30000000 cache ((input |> List.length) + 1) (input |> List.last) 
