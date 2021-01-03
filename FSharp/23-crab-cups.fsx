open System

// let input = "389125467"
let input = "135468729"

type Cup = {
    Value : int
    mutable Left : Cup option
    mutable Right : Cup option
}

let build (input : string) =
    let rec build (input : string) index circleHead circleLast =
        if index >= input.Length
        then circleHead.Left <- Some circleLast
             circleLast.Right <- Some circleHead
             circleHead
        else let newCup = {
                 Value = input.[index].ToString() |> Int32.Parse
                 Left = Some circleLast
                 Right = None
             }
             circleLast.Right <- Some newCup
             build input (index + 1) circleHead newCup

    let firstCup = {
        Value = input.[0].ToString() |> Int32.Parse
        Left = None
        Right = None
    }
    build input 1 firstCup firstCup

let circle = build input

// let minValue = input |> Seq.map (fun c -> c.ToString() |> Int32.Parse) |> Seq.min
// let maxValue = input |> Seq.map (fun c -> c.ToString() |> Int32.Parse) |> Seq.max

let toSeq circle = 
    let mutable current = circle
    seq {
        let mutable dn = false
        while not dn do
            yield current
            current <- current.Right |> Option.get
            if current.Value = circle.Value
            then dn <- true
    }

let rec findDestination minValue maxValue circleSeq value = 
    match circleSeq |> Seq.tryFind (fun c -> c.Value = value) with
    | Some c -> c
    | None -> if value > minValue
              then findDestination minValue maxValue circleSeq (value - 1)
              else findDestination minValue maxValue circleSeq maxValue

let executeMove minValues maxValues current =
    let c1 = current.Right |> Option.get
    let c2 = c1.Right |> Option.get
    let c3 = c2.Right |> Option.get
    let c4 = c3.Right |> Option.get
    current.Right <- Some c4
    c4.Left <- Some current

    let destination = findDestination minValues maxValues (toSeq current) (current.Value - 1)

    let destinationRight = destination.Right |> Option.get
    destination.Right <- Some c1
    c1.Left <- Some destination
    c3.Right <- Some destinationRight
    destinationRight.Left <- Some c3

    current.Right |> Option.get 

let rec executeMoves minValues maxValues n current =
    match n with
    | 0 -> current
    | n -> executeMoves minValues maxValues (n - 1) (executeMove minValues maxValues current)

let print circle =
    printfn "Printing circle"
    String.Join("", (circle |> toSeq |> Seq.map (fun c -> c.Value.ToString()) |> Array.ofSeq))

let result = executeMoves 1 9 100 circle |> print

// Part 2
let addRemaining circle =
    let mutable last = circle.Left |> Option.get
    for i in 10..1000000 do
        last.Right <- Some {
            Value = i
            Left = Some last
            Right = None
        }
        last <- last.Right |> Option.get
    last.Right <- Some circle
    circle.Left <- Some last
    circle

let circle2 = build input  |> addRemaining

let after10m = executeMoves 1 1000000 1000 circle2
    