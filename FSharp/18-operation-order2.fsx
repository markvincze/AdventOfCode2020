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
    | Constant c ->
        Operation {
            Left = Constant c
            Op = op
            Right = newExpr
        }
    | Group e ->
        Operation {
            Left = Group e
            Op = op
            Right = newExpr
        }
    | Operation { Left = left; Op = leftOp; Right = right } ->
        Operation {
            Left = left
            Op = leftOp
            Right = addBottom op right newExpr
        }

let rec parse (line : string) index currentExpr lastOp =
    if index = line.Length
    then (currentExpr |> Option.get, index)
    else let mutable argInt = 0L
         match line.[index] with
         | ' ' -> parse line (index + 1) currentExpr lastOp
         | d when Int64.TryParse(string d, &argInt) ->
             match currentExpr with
             | None -> parse line (index + 1) (Some (Constant argInt)) lastOp
             | Some currentExpr ->
                match lastOp with
                | Add -> parse line (index + 1) (Some (addBottom Add currentExpr (Constant argInt))) lastOp
                | Multiply -> parse line (index + 1) (Some (addTop Multiply currentExpr (Constant argInt))) lastOp
         | '+' -> parse line (index + 1) currentExpr Add
         | '*' -> parse line (index + 1) currentExpr Multiply
         | '(' ->
             let (group, closeIndex) = parse line (index + 1) None lastOp
             match currentExpr with
             | None -> parse line (closeIndex + 1) (Some group) lastOp
             | Some currentExpr ->
                 match lastOp with
                 | Add -> parse line (closeIndex + 1) (Some (addBottom Add currentExpr group)) lastOp
                 | Multiply -> parse line (closeIndex + 1) (Some (addTop Multiply currentExpr group)) lastOp
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

// 7 * 2 + 4 + 3 * 4 * 5
let expr1a = 
    Operation {
        Left = Operation {
            Left = Constant 7L
            Op = Multiply
            Right = Operation {
                Left = Operation {
                    Left = Constant 2L
                    Op = Add
                    Right = Constant 4L
                }
                Op = Add
                Right = Constant 3L
            }
        }
        Op = Multiply
        Right = Operation {
            Left = Constant 4L
            Op = Multiply
            Right = Constant 5L
        }
    }

let value1a = calcExpr expr1a

let expr1b = parse "7 * 2 + 4 + 3 * 4 * 5" 0 None Add |> fst

let value1b = calcExpr expr1b

// 2 * 3 + (4 * 5)
let expr2b = parse "2 * 3 + (4 * 5)" 0 None Add |> fst

let value2b = calcExpr expr2b

let input = File.ReadAllLines "18-operation-order-input.txt"

let calcStr str = parse str 0 None Add |> fst |> calcExpr

let result2 = input
              |> Array.sumBy (fun line -> parse line 0 None Add |> fst |> calcExpr)

// > calcStr "1 + (2 * 3) + (4 * (5 + 6))";;
// val it : int64 = 95L

// > parse "1 + (2 * 3) + (4 * (5 + 6))" 0 None Add |> fst;;