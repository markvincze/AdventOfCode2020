open System
open System.IO
open System.Text.RegularExpressions

type Entry = {
    Parent : string
    Children : (int * string) array
}

let parse (line : string) =
    let segments = line.Split(" contain ")

    {
        Parent = segments.[0].Replace(" bags", "")
        Children =
            match segments.[1] with
            | "no other bags." -> [||]
            | children ->
                children.Split(", ")
                |> Array.map (fun c -> let m = Regex.Match(c, @"^ ?(\d*) (.*) bags?\.?$")
                                       (m.Groups.[1].Value |> Int32.Parse, m.Groups.[2].Value))
    }

let entries = File.ReadAllLines "07-haversacks-input.txt"
              |> Array.map parse
              |> List.ofArray

let rec buildParentCache cache entries =
    match entries with
    | [] -> cache
    | h :: t -> let cache = Array.fold
                                (fun cache (_, c) -> match Map.tryFind c cache with
                                                     | None -> Map.add c [h.Parent] cache
                                                     | Some ps -> Map.add c (h.Parent :: ps) cache)
                                cache
                                h.Children
                buildParentCache cache t

let rec collectAncestors cache color =
    match Map.tryFind color cache with
    | None -> Set.empty<string>
    | Some parents ->
        match parents with
        | [] -> Set.empty<string>
        | parents -> Set.union
                         (parents |> Set.ofList)
                         (List.collect (fun p -> collectAncestors cache p |> Set.toList) parents |> Set.ofList)

let parentCache = buildParentCache Map.empty<string, string list> entries

let result1 = collectAncestors parentCache "shiny gold" |> Set.count

let cache = entries |> List.map (fun e -> (e.Parent, e.Children)) |> Map.ofList

let rec countContainedBags cache color =
    let children = Map.find color cache
    Array.sumBy
        (fun (count, color) -> count + (count * (countContainedBags cache color)))
        children

let result2 = countContainedBags cache "shiny gold"
