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

let buildCache circle =
    circle
    |> toSeq
    |> Seq.map (fun c -> c.Value, c)
    |> Map.ofSeq

let rec findDestination minValue maxValue except cache value = 
    let id = Seq.unfold
                 (fun num -> let next = if num > minValue then num - 1 else maxValue
                             Some (next, next))
                 (value + 1)
             |> Seq.find (fun n -> not (Set.contains n except))

    Map.find id cache

let executeMove minValue maxValue current cache =
    let c1 = current.Right |> Option.get
    let c2 = c1.Right |> Option.get
    let c3 = c2.Right |> Option.get
    let c4 = c3.Right |> Option.get
    current.Right <- Some c4
    c4.Left <- Some current

    let except = Set.empty |> Set.add c1.Value |> Set.add c2.Value |> Set.add c3.Value
    let destination = findDestination minValue maxValue except cache (current.Value - 1)

    let destinationRight = destination.Right |> Option.get
    destination.Right <- Some c1
    c1.Left <- Some destination
    c3.Right <- Some destinationRight
    destinationRight.Left <- Some c3

    current.Right |> Option.get

let rec executeMoves minValue maxValue n current cache =
    match n with
    | 0 -> current
    | n -> executeMoves minValue maxValue (n - 1) (executeMove minValue maxValue current cache) cache

let print circle =
    printfn "Printing circle"
    String.Join("", (circle |> toSeq |> Seq.map (fun c -> c.Value.ToString()) |> Array.ofSeq))

let cache = buildCache circle

let result1 = executeMoves 1 9 2 circle cache |> print

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
let cache2 = buildCache circle2

let after10m = executeMoves 1 1000000 10000000 circle2 cache2

let cache3 = buildCache after10m
let result2 = (int64 cache3.[1].Right.Value.Value) * (int64 cache3.[1].Right.Value.Right.Value.Value)
