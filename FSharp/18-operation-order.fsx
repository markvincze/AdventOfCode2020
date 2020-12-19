open System
open System.IO

type Operation =
| Add
| Multiply

type Expression = {
    Head : (Operation * Expression) option
    Tail : Argument
} and Argument =
| Constant of int
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

// 7 * 2 + 4 + 3 * 4 * 5
let expr = {
    Head = Some (Multiply, {
        Head = Some (Multiply, {
            Head = Some (Add, {
                Head = Some (Add, {
                    Head = Some (Multiply, {
                        Head = None
                        Tail = Constant 7
                    })
                    Tail = Constant 2
                })
                Tail = Constant 4
            })
            Tail = Constant 3
        })
        Tail = Constant 4
    })
    Tail = Constant 5
}

let testValue = calculateExpression expr

// 2 * 3 + (4 * 5)
let expr2 = {
    Head = Some (Add, {
        Head = Some (Multiply, {
            Head = None
            Tail = Constant 2
        })
        Tail = Constant 3
    })
    Tail = Group {
        Head = Some (Multiply, {
            Head = None
            Tail = Constant 4
        })
        Tail = Constant 5
    }
}

let testValue2 = calculateExpression expr2