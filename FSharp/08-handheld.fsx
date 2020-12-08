open System
open System.IO

type Outcome =
| InfiniteLoop of int
| Terminated of int
| OutOfBound of int

type Instruction =
| Nop of int
| Acc of int
| Jmp of int

let parse (line : string) =
    let s = line.Split(' ')
    match s.[0] with
    | "nop" -> Nop (s.[1] |> Int32.Parse)
    | "acc" -> Acc (s.[1] |> Int32.Parse)
    | "jmp" -> Jmp (s.[1] |> Int32.Parse)
    | _ -> failwith "Invalid input"

let instructions = File.ReadAllLines "08-handheld-input.txt"
                   |> Array.map parse

let rec runProgram (instructions : Instruction array) pc acc executed =
    if pc > Array.length instructions
    then OutOfBound acc
    else if pc = Array.length instructions
    then Terminated acc
    else if Set.contains pc executed
         then InfiniteLoop acc
         else match instructions.[pc] with
              | Nop _ -> runProgram instructions (pc + 1) acc (Set.add pc executed)
              | Acc arg -> runProgram instructions (pc + 1) (acc + arg) (Set.add pc executed)
              | Jmp arg -> runProgram instructions (pc + arg) acc (Set.add pc executed)

let result1 = match runProgram instructions 0 0 Set.empty<int> with
              | InfiniteLoop acc -> acc
              | _ -> failwith "Does not result in infinite loop"

let findFixedResult instructions =
    let rec findFixedResult (instructions : Instruction array) index =
        if index >= Array.length instructions
        then failwith "Didn't find a fix for the corruption"
        else let inst = instructions.[index]
             match inst with
             | Jmp _ | Nop _ -> let replacement = match inst with | Jmp x -> Nop x | Nop x -> Jmp x
                                instructions.[index] <- replacement
                                match runProgram instructions 0 0 Set.empty<int> with
                                | Terminated x -> x
                                | _ -> instructions.[index] <- inst
                                       findFixedResult instructions (index + 1)
             | _ -> findFixedResult instructions (index + 1)

    findFixedResult instructions 0

let result2 = findFixedResult instructions
