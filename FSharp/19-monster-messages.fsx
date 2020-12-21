open System
open System.IO
open System.Text.RegularExpressions

type Rule =
| Char of char
| RuleReference of int
| Sequence of Rule list
| Choice of Rule * Rule

let input = File.ReadAllLines "19-monster-messages-input.txt"

let doesMatch ruleCache rule str =
    let rec doesMatch ruleCache rule (str : string) index =
        if index >= str.Length
        then (false, index)
        else match rule with
             | Char char -> match str.[index] with
                            | c when c = char -> true, (index + 1)
                            | _ -> false, index
             | RuleReference id -> let lookedUpRule = Map.find id ruleCache
                                   doesMatch ruleCache lookedUpRule str index
             | Choice (r1, r2) -> let m1, i1 = doesMatch ruleCache r1 str index
                                  let m2, i2 = doesMatch ruleCache r2 str index
                                  if m1 then m1, i1
                                  else if m2 then m2, i2
                                  else false, index
             | Sequence rules -> List.fold
                                     (fun (m, i) r -> if m then doesMatch ruleCache r str i
                                                      else m, i)
                                     (true, index)
                                     rules

    let (m, i) = doesMatch ruleCache rule str 0
    m && i = (str.Length)

let ids (line : string) = let ruleStr = (line.Split ": ").[1]
                        //   printfn "Rule: %s" ruleStr
                          if ruleStr.StartsWith "\"" 
                          then [||]
                          else ruleStr.Split([| ' '; '|' |], StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                               |> Array.map Int32.Parse

let parseSeq ruleCache (str : string) =
    str.Split ' '
    // |> Array.map (fun rid -> Map.find (Int32.Parse rid) ruleCache)
    // |> Array.map (fun rid -> RuleReference (Int32.Parse rid))
    |> Array.map (Int32.Parse >> RuleReference)
    |> List.ofArray
    |> Sequence

let rec parse (rulesInput : string list) (ruleCache : Map<int, Rule>) =
    // let nextLine =
    //     rulesInput
    //     |> Array.tryFind (fun line -> let ruleId = (line.Split ':').[0] |> Int32.Parse
    //                                   not (Map.containsKey ruleId ruleCache) &&
    //                                   ids line |> Array.forall (fun i -> Map.containsKey i ruleCache))
    // printfn "Start parsing line '%s'" (Option.get nextLine)
    // match nextLine with
    // | None -> ruleCache.[0], ruleCache
    // | Some line ->
    match rulesInput with
    | [] -> ruleCache.[0], ruleCache
    | line :: t ->
        let segments = line.Split ": "
        let ruleId = segments.[0] |> Int32.Parse
        let ruleStr = segments.[1]
        let rule = if ruleStr.StartsWith "\""
                   then Char (ruleStr.Trim '"').[0]
                   else if ruleStr.Contains "|"
                   then let [|seq1; seq2|] = ruleStr.Split " | "
                        Choice (parseSeq ruleCache seq1, parseSeq ruleCache seq2)
                   else parseSeq ruleCache ruleStr
        // printfn "Adding rule with id %d to the cache" ruleId
        parse t (Map.add ruleId rule ruleCache)

let rulesInput = input
                 |> Array.takeWhile (fun l -> l <> "")
                 |> List.ofArray

let rootRule, ruleCache = parse rulesInput Map.empty<int, Rule>

let testInput = input
                |> Array.skipWhile (fun l -> l <> "")
                |> Array.skip 1

let result1 = testInput
              |> Seq.filter (doesMatch ruleCache rootRule)
              |> Seq.length
// Result1: 156