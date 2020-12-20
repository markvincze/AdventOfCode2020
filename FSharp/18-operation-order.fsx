open System
open System.IO

type Operator =
| Add
| Multiply

type Expression =
| Constant of int64
| Operation of Operation
| Group of Expression
and Operation = {
    Left : Expression
    Op : Operator
    Right : Expression
}

let addTop op left right =
    Operation {
        Left = left
        Op = op
        Right = right
    }

let rec addBottom op expr newExpr =
    match expr with
    | Constant _ | Group _ ->
        Operation {
            Left = expr
            Op = op
            Right = newExpr
        }
    | Operation { Left = left; Op = leftOp; Right = right } ->
        Operation {
            Left = left
            Op = leftOp
            Right = addBottom op right newExpr
        }

let rec parse addAdd addMultiply (line : string) index currentExpr lastOp =
    if index = line.Length
    then (currentExpr |> Option.get, index)
    else let mutable argInt = 0L
         match line.[index] with
         | ' ' -> parse addAdd addMultiply line (index + 1) currentExpr lastOp
         | d when Int64.TryParse(string d, &argInt) ->
             match currentExpr with
             | None -> parse addAdd addMultiply line (index + 1) (Some (Constant argInt)) lastOp
             | Some currentExpr ->
                 let addFn = match lastOp with Add -> addAdd | Multiply -> addMultiply
                 parse addAdd addMultiply line (index + 1) (Some (addFn currentExpr (Constant argInt))) lastOp
         | '+' -> parse addAdd addMultiply line (index + 1) currentExpr Add
         | '*' -> parse addAdd addMultiply line (index + 1) currentExpr Multiply
         | '(' ->
             let (group, closeIndex) = parse addAdd addMultiply line (index + 1) None lastOp
             match currentExpr with
             | None -> parse addAdd addMultiply line (closeIndex + 1) (Some group) lastOp
             | Some currentExpr ->
                 let addFn = match lastOp with Add -> addAdd | Multiply -> addMultiply
                 parse addAdd addMultiply line (closeIndex + 1) (Some (addFn currentExpr group)) lastOp
         | ')' -> (currentExpr |> Option.get |> Group, index)
         | _ -> failwith "Invalid input"

let rec calcExpr expr = 
    match expr with
    | Constant c -> c
    | Group e -> calcExpr e
    | Operation { Left = left; Op = op; Right = right } ->
        match op with
        | Add -> calcExpr left + calcExpr right
        | Multiply -> calcExpr left * calcExpr right

let input = File.ReadAllLines "18-operation-order-input.txt"

let result1 = input
              |> Array.sumBy (fun line -> parse (addTop Add) (addTop Multiply) line 0 None Add |> fst |> calcExpr)

let result2 = input
              |> Array.sumBy (fun line -> parse (addBottom Add) (addTop Multiply) line 0 None Add |> fst |> calcExpr)