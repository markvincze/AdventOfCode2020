open System
open System.IO
open System.Text.RegularExpressions
open System.Text

type Instruction =
| Mask of string
| Write of (int64 * int64)

let parse (line : string) =
    if line.StartsWith("mask")
    then Mask (line.Substring 7)
    else let rMatch = Regex.Match (line, @"^mem\[(\d+)\] = (\d+)$")
         Write (rMatch.Groups.[1].Value |> Int64.Parse, rMatch.Groups.[2].Value |> Int64.Parse)

let instructions = File.ReadAllLines "14-docking-data-input.txt"
                   |> Array.map parse
                   |> List.ofArray

let processInstruction1 instruction memory (mask : string) =
    match instruction with
    | Mask m -> memory, m
    | Write (addr, value) ->
        let valueBin = Convert.ToString(value, 2).PadLeft(36, '0')
        let sb = StringBuilder()
        for i in 0..35 do
            match mask.[i] with
            | 'X' -> sb.Append valueBin.[i]
            | m -> sb.Append m
            |> ignore

        Map.add addr (Convert.ToInt64(sb.ToString(), 2)) memory, mask

let rec processProgram1 instructions memory mask =
    match instructions with
    | [] -> memory
    | h :: t -> let memory, mask = processInstruction1 h memory mask
                processProgram1 t memory mask

let finalMemory1 = processProgram1 instructions Map.empty<int64, int64> "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

let result1 = finalMemory1 |> Seq.sumBy (fun kvp -> kvp.Value)

let rec allSubsets items =
    match items with
    | [] -> Seq.ofList [ [] ]
    | h :: t -> let tSubsets = allSubsets t
                seq {
                    yield! allSubsets t
                    for s in tSubsets do
                        yield h :: s
                }

let allAddresses (address : int64) (mask : string) =
    let valueBin = Convert.ToString(address, 2).PadLeft(36, '0') |> StringBuilder
    mask |> Seq.iteri (fun i c -> if c = '1' then valueBin.[i] <- '1')

    let xIndexes = mask |> Seq.indexed |> Seq.filter (fun (i, c) -> c = 'X') |> Seq.map fst |> List.ofSeq

    seq {
        for ones in allSubsets xIndexes do
            ones |> List.iter (fun i -> valueBin.[i] <- '1')
            xIndexes |> List.except ones |> List.iter (fun i -> valueBin.[i] <- '0')
            yield Convert.ToInt64(valueBin.ToString(), 2)
    }

let processInstruction2 instruction memory (mask : string) =
    match instruction with
    | Mask m -> memory, m
    | Write (addr, value) ->
        let addresses = allAddresses addr mask
        let newMemory =
            addresses
            |> Seq.fold (fun mem addr -> Map.add addr value mem) memory
        newMemory, mask

let rec processProgram2 instructions memory mask =
    match instructions with
    | [] -> memory
    | h :: t -> let memory, mask = processInstruction2 h memory mask
                processProgram2 t memory mask

let finalMemory2 = processProgram2 instructions Map.empty<int64, int64> "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

let result2 = finalMemory2 |> Seq.sumBy (fun kvp -> kvp.Value)