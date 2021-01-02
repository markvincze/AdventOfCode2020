open System
open System.IO

type Food = {
    Ingredients : string list
    Allergens : string list
}

// let input = [|
//     "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
//     "trh fvjkl sbzzf mxmxvkd (contains dairy)"
//     "sqjhc fvjkl (contains soy)"
//     "sqjhc mxmxvkd sbzzf (contains fish)"
// |]

let input = File.ReadAllLines "21-allergen-input.txt"

let parse (line : string) = let sections = line.TrimEnd(')').Split(" (contains ")
                            {
                                Ingredients = sections.[0].Split(" ") |> List.ofArray
                                Allergens = sections.[1].Split(", ") |> List.ofArray
                            }

let foods = input
            |> Array.map parse
            |> List.ofArray

let findNextPairing allAllergens foods =
    let nextPairing = 
        allAllergens
        |> Set.map (fun allergen ->
            let candidates =
                foods
                |> List.fold
                    (fun candidates food ->
                        match candidates with
                        | None -> if food.Allergens |> List.contains allergen
                                  then food.Ingredients |> Set.ofList |> Some
                                  else None
                        | Some cs -> if food.Allergens |> List.contains allergen
                                     then Set.intersect (food.Ingredients |> Set.ofList) cs |> Some
                                     else candidates)
                    None
            (allergen, candidates))
        |> Seq.tryFind (fun (allergen, candidates) -> match candidates with
                                                      | None -> failwith "This allergen has no candidates!"
                                                      | Some cs -> (cs |> Set.count) = 1)
    match nextPairing with
    | None -> failwith "Couldn't find deterministic pairing"
    | Some (allergen, candidates) -> allergen, Seq.head (Option.get candidates)

let rec findPairings foods pairings =
    let allAllergens = foods |> List.collect (fun f -> f.Allergens) |> Set.ofList
    if allAllergens |> Set.count = 0
    then pairings
    else let (allergen, food) = findNextPairing allAllergens foods
         let newFoods = foods
                        |> List.map (fun f -> {
                                Ingredients = f.Ingredients |> List.except [ food ]
                                Allergens = f.Allergens |> List.except [ allergen ]
                            })
         findPairings newFoods (Map.add allergen food pairings)

let pairings = findPairings foods Map.empty

let nonAllergenIngredients = (pairings |> Seq.map (fun kvp -> kvp.Value) |> Set.ofSeq)
                             |> Set.difference
                                 (foods
                                 |> Seq.collect (fun f -> f.Ingredients)
                                 |> Set.ofSeq)

let result1 = foods
              |> List.sumBy (fun f -> f.Ingredients
                                      |> List.filter (fun i -> Set.contains i nonAllergenIngredients)
                                      |> List.length)

let dangerousList = pairings |> Seq.sortBy (fun kvp -> kvp.Key) |> Seq.map (fun kvp -> kvp.Value) |> List.ofSeq

let result2 = String.Join(',', dangerousList)
