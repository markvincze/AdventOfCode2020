open System
open System.IO

type Rule =
| Char of char
| RuleReference of int
| Sequence of Rule list
| Choice of Rule * Rule

let input = File.ReadAllLines "19-monster-messages-input.txt"

let doesMatch ruleCache rule str =
    let rec doesMatch ruleCache rule (str : string) index =
        if index >= str.Length
        then []
        else match rule with
             | Char char -> match str.[index] with
                            | c when c = char -> [index + 1]
                            | _ -> []
             | RuleReference id -> let lookedUpRule = Map.find id ruleCache
                                   doesMatch ruleCache lookedUpRule str index
             | Choice (r1, r2) -> let ms1 = doesMatch ruleCache r1 str index
                                  let ms2 = doesMatch ruleCache r2 str index
                                  List.append ms1 ms2
             | Sequence rules -> List.fold
                                     (fun indices rule ->
                                        indices
                                        |> List.collect (fun i -> doesMatch ruleCache rule str i))
                                     [index]
                                     rules

    let indices = doesMatch ruleCache rule str 0
    indices |> List.exists (fun i -> i = (str.Length))

let ids (line : string) = let ruleStr = (line.Split ": ").[1]
                          if ruleStr.StartsWith "\"" 
                          then [||]
                          else ruleStr.Split([| ' '; '|' |], StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                               |> Array.map Int32.Parse

let parseSeq ruleCache (str : string) =
    str.Split ' '
    |> Array.map (Int32.Parse >> RuleReference)
    |> List.ofArray
    |> Sequence

let rec parse (rulesInput : string list) (ruleCache : Map<int, Rule>) =
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
        parse t (Map.add ruleId rule ruleCache)

let testInput = input
                |> Array.skipWhile (fun l -> l <> "")
                |> Array.skip 1

let rulesInput1 = input
                 |> Array.takeWhile (fun l -> l <> "")
                 |> List.ofArray

let rootRule1, ruleCache1 = parse rulesInput1 Map.empty<int, Rule>

let rulesInput2 = input
                 |> Array.takeWhile (fun l -> l <> "")
                 |> Array.map (fun line ->
                    match line with
                    | "8: 42" -> "8: 42 | 42 8"
                    | "11: 42 31" -> "11: 42 31 | 42 11 31"
                    | _ -> line)
                 |> List.ofArray

let rootRule2, ruleCache2 = parse rulesInput2 Map.empty<int, Rule>

let result1 = testInput
              |> Seq.filter (doesMatch ruleCache1 rootRule1)
              |> Seq.length

let result2 = testInput
              |> Seq.filter (doesMatch ruleCache2 rootRule2)
              |> Seq.length
