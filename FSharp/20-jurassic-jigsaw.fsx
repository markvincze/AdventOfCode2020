open System
open System.IO
open System.Text.RegularExpressions
open System.Text

let input = File.ReadAllLines "20-jurassic-jigsaw-input copy.txt"
            |> List.ofArray

type Border = Top | Right | Bottom | Left

type Flip = Original | Horizontal | Vertical | HorizontalVertical

type Transformation = {
    Rotation : int
    Flip : Flip
}

type Tile = {
    Id : int
    TopBorder : string
    RightBorder : string
    BottomBorder : string
    LeftBorder : string
    BorderCache : Map<Border * Transformation, int>
}

let toString chars =
    let sb = StringBuilder()
    for c in chars do
        sb.Append (c.ToString()) |> ignore

    sb.ToString()

let reverse str =
    str |> Seq.toList |> List.rev |> toString

let allTransformations = [ 0; 90; 180; 270 ]
                         |> List.collect (fun rot -> [ { Rotation = rot; Flip = Original }
                                                       { Rotation = rot; Flip = Horizontal }
                                                       { Rotation = rot; Flip = Vertical } ])
                                                       //{ Rotation = rot; Flip = HorizontalVertical } ])

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

let parseTile idLine lines = 
    let id = (Regex.Match(idLine, "Tile (\\d+):").Groups.[1].Value) |> Int32.Parse
    let top = List.head lines |> toString
    let bottom = List.last lines |> Seq.rev |> toString
    let left = lines |> List.map Seq.head |> Seq.rev |> toString
    let right = lines |> List.map Seq.last |> toString

    let tile = {
        Id = id
        TopBorder = top
        RightBorder = right
        BottomBorder = bottom
        LeftBorder = left
        BorderCache = Map.empty
    }

    { tile with
        BorderCache = [ Top; Right; Bottom; Left]
                      |> List.collect (
                          fun b -> allTransformations
                                   |> List.map (fun t ->
                                       let num =
                                           match b with
                                           | Top | Right -> (border t tile b).Replace('.', '0').Replace('#', '1')
                                           | Bottom | Left -> (border t tile b).Replace('.', '0').Replace('#', '1') |> reverse
                                       (b, t), Convert.ToInt32(num, 2)
                                   )
                      )
                      |> Map.ofList
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

// let arraySet (x, y) item array =
//     let copy = Array2D.copy array
//     copy.[x, y] <- item
//     copy

let tileAt (x, y) image =
    if x >= 0 && x < Array2D.length1 image && y >= 0 && y < Array2D.length2 image
    then image.[x, y]
    else None

let fits image (tile, transformation) (x, y) =
    // (match tileAt (x, y - 1) image with None -> true | Some (above, aboveTransformation) -> (border transformation tile Top |> reverse = border aboveTransformation above Bottom)) &&
    // (match tileAt (x + 1, y) image with None -> true | Some (right, rightTransformation) -> (border transformation tile Right |> reverse = border rightTransformation right Left)) &&
    // (match tileAt (x, y + 1) image with None -> true | Some (below, belowTransformation) -> (border transformation tile Bottom |> reverse = border belowTransformation below Top)) &&
    // (match tileAt (x - 1, y) image with None -> true | Some (left, leftTransformation) -> (border transformation tile Left |> reverse = border leftTransformation left Right))
    (match tileAt (x, y - 1) image with None -> true | Some (above, aboveTransformation) -> tile.BorderCache.[(Top, transformation)] = above.BorderCache.[(Bottom, aboveTransformation)]) &&
    (match tileAt (x + 1, y) image with None -> true | Some (right, rightTransformation) -> tile.BorderCache.[(Right, transformation)] = right.BorderCache.[(Left, rightTransformation)]) &&
    (match tileAt (x, y + 1) image with None -> true | Some (below, belowTransformation) -> tile.BorderCache.[(Bottom, transformation)] = below.BorderCache.[(Top, belowTransformation)]) &&
    (match tileAt (x - 1, y) image with None -> true | Some (left, leftTransformation) -> tile.BorderCache.[(Left, transformation)] = left.BorderCache.[(Right, leftTransformation)])

let rec findSolution image (x, y) remainingTiles =
    // if y >= 4 && x > 0 then printfn "Trying to find solutions, current pos: %d, %d" x y
    if y >= 8 then printfn "Trying to find solutions, current pos: %d, %d" x y
    if y >= Array2D.length2 image
    // if y >= 4
    then //printfn "Reached solution at %d, %d" x y
        //  [ image ]
         Some (image |> Array2D.copy)
    else let result = remainingTiles
                      |> Seq.collect (fun tile -> allTransformations |> List.map (fun transformation -> tile, transformation))
                      |> Seq.filter (fun (tile, transformation) -> fits image (tile, transformation) (x, y))
                      |> Seq.tryPick (fun (tile, transformation) -> //let newImage = arraySet (x, y) (Some (tile, transformation)) image
                                                                    image.[x, y] <- (Some (tile, transformation))
                                                                    // findSolution image (next (x, y)) (List.except [tile] remainingTiles))
                                                                    findSolution image (next (x, y)) (Set.remove tile remainingTiles))
         image.[x, y] <- None
         result

let (emptyImage : ((Tile * Transformation) option)[,]) = Array2D.create sideTileCount sideTileCount None

let result = findSolution emptyImage (0, 0) (tiles |> Set.ofList)

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
                let tile, transformation = image.[tileX, tileY] |> Option.get
                let tile = transform transformation tile

                if y = 0 then printf "%s" tile.TopBorder
                else if y = tileSize - 1 then printf "%s" (reverse tile.BottomBorder)
                else if y = 2 then printf "%c #%d  %c" (tile.LeftBorder.[y]) tile.Id (tile.RightBorder.[tileSize - 1 - y])
                else printf "%c        %c" (tile.LeftBorder.[y]) (tile.RightBorder.[tileSize - 1 - y])
                printf " "
            printfn ""
        printfn ""

