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

let processInstruction instruction memory (mask : string) =
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

let rec processProgram instructions memory mask =
    match instructions with
    | [] -> memory
    | h :: t -> let memory, mask = processInstruction h memory mask
                processProgram t memory mask

let finalMemory = processProgram instructions Map.empty<int64, int64> "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

let result1 = finalMemory |> Seq.sumBy (fun kvp -> kvp.Value)
