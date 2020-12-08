open System
open System.IO

type Outcome =
| InfiniteLoop of int
| Terminated of int
| OutOfBound of int

type Instruction =
| Nop
| Acc of int
| Jmp of int

let parse (line : string) =
    let s = line.Split(' ')
    match s.[0] with
    | "nop" -> Nop
    | "acc" -> Acc (s.[1] |> Int32.Parse)
    | "jmp" -> Jmp (s.[1] |> Int32.Parse)
    | _ -> failwith "Invalid input"

let instructions = File.ReadAllLines "08-handheld-input.txt"
                   |> Array.map parse

let rec runUntilRepeat (instructions : Instruction array) pc acc executed =
    if pc > Array.length instructions
    then OutOfBound acc
    else if pc = Array.length instructions
    then Terminated acc
    else if Set.contains pc executed
         then InfiniteLoop acc
         else match instructions.[pc] with
              | Nop -> runUntilRepeat instructions (pc + 1) acc (Set.add pc executed)
              | Acc arg -> runUntilRepeat instructions (pc + 1) (acc + arg) (Set.add pc executed)
              | Jmp arg -> runUntilRepeat instructions (pc + arg) acc (Set.add pc executed)

let result1 = match runUntilRepeat instructions 0 0 Set.empty<int> with
              | InfiniteLoop acc -> acc
              | _ -> failwith "Does not result in infinite loop"

