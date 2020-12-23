open System
open System.IO
open System.Text.RegularExpressions
open System.Text

let input = File.ReadAllLines "20-jurassic-jigsaw-input.txt"
            |> List.ofArray

type Boder = Top | Right | Bottom | Left

type Tile = {
    Id : int
    TopBorder : string
    RightBorder : string
    BottomBorder : string
    LeftBorder : string
}

type Flip = Original | Horizontal | Vertical | HorizontalVertical

type Transformation = {
    Rotation : int
    Flip : Flip
}

let toString chars =
    let sb = StringBuilder()
    for c in chars do
        sb.Append (c.ToString()) |> ignore

    sb.ToString()

let reverse str =
    str |> Seq.toList |> List.rev |> toString

let parseTile idLine lines = 
    let id = (Regex.Match(idLine, "Tile (\\d+):").Groups.[1].Value) |> Int32.Parse
    let top = List.head lines |> toString
    let bottom = List.last lines |> Seq.rev |> toString
    let left = lines |> List.map Seq.head |> Seq.rev |> toString
    let right = lines |> List.map Seq.last |> toString

    {
        Id = id
        TopBorder = top
        RightBorder = right
        BottomBorder = bottom
        LeftBorder = left
    }

let rec parse input tiles = 
    let input = input |> List.skipWhile (fun l -> l = "")
    match input with
    | [] -> tiles
    | h :: t -> let tile = List.take 10 t
                           |> parseTile h
                parse (List.skip 10 t) (tile :: tiles)

let tiles = parse input List.empty
let sideTileCount = tiles |> List.length |> float |> Math.Sqrt |> int
let tileSize = (tiles |> List.head).BottomBorder.Length

let next (x, y) = if x < sideTileCount - 1
                  then (x + 1, y)
                  else (0, y + 1)

let arraySet (x, y) item array =
    let copy = Array2D.copy array
    copy.[x, y] <- item
    copy

let rotate rotation tile =
    match rotation with
    | 0 -> tile
    | 90 -> { tile with TopBorder = tile.LeftBorder; RightBorder = tile.TopBorder; BottomBorder = tile.RightBorder; LeftBorder = tile.BottomBorder }
    | 180 -> { tile with TopBorder = tile.BottomBorder; RightBorder = tile.LeftBorder; BottomBorder = tile.TopBorder; LeftBorder = tile.RightBorder }
    | 270 -> { tile with TopBorder = tile.RightBorder; RightBorder = tile.BottomBorder; BottomBorder = tile.LeftBorder; LeftBorder = tile.TopBorder }
    | _ -> failwith "Invalid rotation"

let flip flip tile =
    match flip with
    | Original -> tile
    | Horizontal -> { tile with TopBorder = reverse tile.TopBorder; RightBorder = reverse tile.LeftBorder; BottomBorder = reverse tile.BottomBorder; LeftBorder = reverse tile.RightBorder }
    | Vertical -> { tile with TopBorder = reverse tile.BottomBorder; RightBorder = reverse tile.RightBorder; BottomBorder = reverse tile.TopBorder; LeftBorder = reverse tile.LeftBorder }
    | HorizontalVertical -> { tile with TopBorder = tile.BottomBorder; RightBorder = tile.LeftBorder; BottomBorder = tile.TopBorder; LeftBorder = tile.RightBorder }

let transform transformation tile =
    tile |> rotate transformation.Rotation |> flip transformation.Flip

let border transformation tile border =
    let tile = tile |> transform transformation

    match border with
    | Top -> tile.TopBorder
    | Right -> tile.RightBorder
    | Bottom -> tile.BottomBorder
    | Left -> tile.LeftBorder

let tileAt (x, y) image =
    if x >= 0 && x < Array2D.length1 image && y >= 0 && y < Array2D.length2 image
    then image.[x, y]
    else None

let fits image (tile, transformation) (x, y) =
    (match tileAt (x, y - 1) image with None -> true | Some (above, aboveTransformation) -> (border transformation tile Top |> reverse = border aboveTransformation above Bottom)) &&
    (match tileAt (x + 1, y) image with None -> true | Some (right, rightTransformation) -> (border transformation tile Right |> reverse = border rightTransformation right Left)) &&
    (match tileAt (x, y + 1) image with None -> true | Some (below, belowTransformation) -> (border transformation tile Bottom |> reverse = border belowTransformation below Top)) &&
    (match tileAt (x - 1, y) image with None -> true | Some (left, leftTransformation) -> (border transformation tile Left |> reverse = border leftTransformation left Right))

let allTransformations = [ 0; 90; 180; 270 ]
                         |> List.collect (fun rot -> [ { Rotation = rot; Flip = Original }
                                                       { Rotation = rot; Flip = Horizontal }
                                                       { Rotation = rot; Flip = Vertical } ])
                                                       //{ Rotation = rot; Flip = HorizontalVertical } ])

let rec findSolution image (x, y) remainingTiles =
    // if y >= 1 then printfn "Trying to find solutions, current pos: %d, %d" x y
    if y >= Array2D.length2 image
    then //printfn "Reached solution at %d, %d" x y
         [ image ]
    else remainingTiles
         |> List.collect (fun tile -> allTransformations |> List.map (fun transformation -> tile, transformation))
         |> List.filter (fun (tile, transformation) -> fits image (tile, transformation) (x, y))
         |> List.collect (fun (tile, transformation) -> let newImage = arraySet (x, y) (Some (tile, transformation)) image
                                                        findSolution newImage (next (x, y)) (List.except [tile] remainingTiles))

let (emptyImage : ((Tile * Transformation) option)[,]) = Array2D.create sideTileCount sideTileCount None

let result = findSolution emptyImage (0, 0) tiles

let validate image =
    seq {
        for x in 0..sideTileCount - 1 do
            for y in 0..sideTileCount - 1 do
                (x, y)
    }
    |> Seq.forall (fun (x, y) -> fits image (Option.get image.[x, y]) (x, y ))

let printImage (image : (Tile * Transformation) option [,]) =
    for tileY in 0..sideTileCount - 1 do
        for y in 0..tileSize - 1 do
            for tileX in 0..sideTileCount - 1 do
                // printfn "Getting tile %d, %d" tileX tileY
                let tile, transformation = image.[tileX, tileY] |> Option.get
                let tile = transform transformation tile

                if y = 0 then printf "%s" tile.TopBorder
                else if y = tileSize - 1 then printf "%s" (reverse tile.BottomBorder)
                else if y = 2 then printf "%c #%d   %c" (tile.LeftBorder.[y]) tile.Id (tile.RightBorder.[tileSize - 1 - y])
                else printf "%c        %c" (tile.LeftBorder.[y]) (tile.RightBorder.[tileSize - 1 - y])
                printf " "
            printfn ""
        printfn ""
    // failwith "boop" |> ignore

printImage (List.head result);;




// let tile1951 = tiles |> List.find (fun t -> t.Id = 1951)
// let tile2311 = tiles |> List.find (fun t -> t.Id = 2311)
