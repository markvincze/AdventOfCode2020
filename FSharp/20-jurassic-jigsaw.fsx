open System
open System.IO
open System.Text.RegularExpressions
open System.Text

let input = File.ReadAllLines "20-jurassic-jigsaw-input.txt"
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
    FittingTiles : Map<Border * Transformation, Set<(Transformation * int)>>
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
        FittingTiles = Map.empty
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

let opposite border = match border with
                      | Top -> Bottom
                      | Right -> Left
                      | Bottom -> Top
                      | Left -> Right

let preloadFittingTiles (tiles : Tile list) =
    tiles
    |> List.map (fun tileOrig ->
        let fittingTiles =
            [ Top; Right; Bottom; Left]
            |> List.collect (fun b -> allTransformations |> List.map (fun t -> (b, t)))
            |> List.map (fun (borderOrig, transformOrig) ->
                let fittingTiles =
                    allTransformations
                    |> List.collect (fun transformOther -> tiles |> List.map (fun tileOther -> (transformOther, tileOther)))
                    |> List.filter (fun (transformOther, tileOther) -> tileOrig.Id <> tileOther.Id && tileOrig.BorderCache.[(borderOrig, transformOrig)] = tileOther.BorderCache.[((opposite borderOrig), transformOther)])
                (borderOrig, transformOrig), (fittingTiles |> List.map (fun (trans, tile) -> (trans, tile.Id)) |> Set.ofList))
            |> Map.ofList

        { tileOrig with FittingTiles = fittingTiles})

let tiles = parse input List.empty |> preloadFittingTiles

let sideTileCount = tiles |> List.length |> float |> Math.Sqrt |> int
let tileSize = (tiles |> List.head).BottomBorder.Length

let next (x, y) = if x < sideTileCount - 1
                  then (x + 1, y)
                  else (0, y + 1)

let tileCache = tiles |> List.map (fun tile -> (tile.Id, tile)) |> Map.ofList

let rec findSolution image (x, y) remainingTiles =
    if y >= Array2D.length2 image
    then printfn "Reached solution at %d, %d" x y
         Some (image |> Array2D.copy)
    else let result = allTransformations
                      |> Seq.collect (fun transform ->
                          if (x, y) = (0, 0) then remainingTiles |> Seq.map (fun rt -> (rt, transform))
                          else let fittingTiles = if y = 0 then let leftTile, leftTransform = (Option.get image.[x - 1, y])
                                                                leftTile.FittingTiles.[(Right, leftTransform)]
                                                  else if x = 0 then let aboveTile, aboveTransform = (Option.get image.[x, y - 1])
                                                                     aboveTile.FittingTiles.[(Bottom, aboveTransform)]
                                                  else let leftTile, leftTransform = (Option.get image.[x - 1, y])
                                                       let aboveTile, aboveTransform = (Option.get image.[x, y - 1])
                                                       Set.intersect leftTile.FittingTiles.[(Right, leftTransform)] aboveTile.FittingTiles.[(Bottom, aboveTransform)]
                               fittingTiles
                               |> Seq.filter (fun (tileTransform, _) -> tileTransform = transform)
                               |> Seq.map (fun (tileTransform, tileId) -> (Map.find tileId tileCache), tileTransform))
                      |> Seq.tryPick (fun (tile, transformation) -> image.[x, y] <- (Some (tile, transformation))
                                                                    findSolution image (next (x, y)) (Set.remove tile remainingTiles))
         image.[x, y] <- None
         result

let (emptyImage : ((Tile * Transformation) option)[,]) = Array2D.create sideTileCount sideTileCount None

let solution = findSolution emptyImage (0, 0) (tiles |> Set.ofList)

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

// Part 2

let parseTileRaw idLine lines = 
    let id = (Regex.Match(idLine, "Tile (\\d+):").Groups.[1].Value) |> Int32.Parse
    id, lines

let rec parseRaw input tiles = 
    let input = input |> List.skipWhile (fun l -> l = "")
    match input with
    | [] -> tiles |> Map.ofList
    | h :: t -> let tile = List.take 10 t
                           |> parseTileRaw h
                parseRaw (List.skip 10 t) (tile :: tiles)

let rawTiles = parseRaw input List.empty

let rotateRaw rotation (tile : string list) =
    match rotation with
    | 0 -> tile
    | 90 -> [0..9] |> List.map (fun i -> tile |> List.map (fun line -> line.[i]) |> toString |> reverse)
    | 180 -> tile |> List.map (fun line -> line |> reverse) |> List.rev
    | 270 -> [9..-1..0] |> List.map (fun i -> tile |> List.map (fun line -> line.[i]) |> toString)
    | _ -> failwith "Invalid rotation"

let flipRaw flip (tile : string list) =
    match flip with
    | Original -> tile
    | Horizontal -> tile |> List.map (fun line -> line |> reverse)
    | Vertical -> tile |> List.rev
    | HorizontalVertical -> tile |> List.map (fun line -> line |> reverse) |> List.rev

let transformRaw transformation tile =
    tile |> rotateRaw transformation.Rotation |> flipRaw transformation.Flip

let printImage2 (image : (Tile * Transformation) option [,]) =
    for tileY in 0..sideTileCount - 1 do
        for y in 1..tileSize - 2 do
            for tileX in 0..sideTileCount - 1 do
                let tile, transformation = image.[tileX, tileY] |> Option.get
                let rawTile = rawTiles.[tile.Id] |> transformRaw transformation

                printf "%s" ((rawTile |> List.item y).Substring(1, 8))
            printfn ""