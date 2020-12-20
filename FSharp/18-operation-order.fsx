open System
open System.IO

type Operation =
| Add
| Multiply

type Expression = {
    Head : (Operation * Expression) option
    Tail : Argument
} and Argument =
| Constant of int64
| Group of Expression

let rec calculateExpression expr =
    let tailValue = calculateArgument expr.Tail
    match expr.Head with
    | None -> tailValue
    | Some (op, headExpr) -> let headValue = calculateExpression headExpr
                             match op with
                             | Add -> headValue + tailValue
                             | Multiply -> headValue * tailValue
and calculateArgument arg =
    match arg with
    | Constant x -> x
    | Group expr -> calculateExpression expr

let input = File.ReadAllLines "18-operation-order-input.txt"

// 7 * 2 + 4 + 3 * 4 * 5
let rec parse (line : string) index currentExpr lastOp = 
    if index = line.Length
    then (currentExpr |> Option.get, index)
    else let mutable argInt = 0L
         match line.[index] with
         | ' ' -> parse line (index + 1) currentExpr lastOp
         | d when Int64.TryParse(string d, &argInt) ->
             match currentExpr with
             | None -> parse line (index + 1) (Some { Head = None; Tail = Constant argInt }) lastOp
             | Some currentExpr -> let newExpr = { 
                                       Head = Some (lastOp, currentExpr);
                                       Tail = Constant argInt
                                   }
                                   parse line (index + 1) (Some newExpr) lastOp
         | '+' -> parse line (index + 1) currentExpr Add
         | '*' -> parse line (index + 1) currentExpr Multiply
         | '(' ->
             let (group, closeIndex) = parse line (index + 1) None lastOp
             match currentExpr with
             | None -> parse line (closeIndex + 1) (Some { Head = None; Tail = Group group }) lastOp
             | Some currentExpr -> let newExpr = { 
                                       Head = Some (lastOp, currentExpr);
                                       Tail = Group group
                                   }
                                   parse line (closeIndex + 1) (Some newExpr) lastOp
         | ')' -> (currentExpr |> Option.get, index)
         | _ -> failwith "Invalid input"

// 7 * 2 + 4 + 3 * 4 * 5
let expr1a = {
    Head = Some (Multiply, {
        Head = Some (Multiply, {
            Head = Some (Add, {
                Head = Some (Add, {
                    Head = Some (Multiply, {
                        Head = None
                        Tail = Constant 7L
                    })
                    Tail = Constant 2L
                })
                Tail = Constant 4L
            })
            Tail = Constant 3L
        })
        Tail = Constant 4L
    })
    Tail = Constant 5L
}

let testValue1a = calculateExpression expr1a

let expr1b, _ = parse "7 * 2 + 4 + 3 * 4 * 5" 0 None Add

let testValue1b = calculateExpression expr1b

// 2 * 3 + (4 * 5)
let expr2a = {
    Head = Some (Add, {
        Head = Some (Multiply, {
            Head = None
            Tail = Constant 2L
        })
        Tail = Constant 3L
    })
    Tail = Group {
        Head = Some (Multiply, {
            Head = None
            Tail = Constant 4L
        })
        Tail = Constant 5L
    }
}

let expr2b, _ = parse "2 * 3 + (4 * 5)" 0 None Add

let testValue2a = calculateExpression expr2a

let testValue2b = calculateExpression expr2b

let result1 = input
              |> Array.sumBy (fun line -> parse line 0 None Add |> fst |> calculateExpression)