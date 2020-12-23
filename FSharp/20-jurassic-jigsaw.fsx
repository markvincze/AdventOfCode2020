open System
open System.IO
open System.Text.RegularExpressions
open System.Text

let input = File.ReadAllLines "20-jurassic-jigsaw-input.txt"
            |> List.ofArray

type Boder = Top | Right | Bottom | Left

type Square = {
    TopBorder: string
    RightBorder: string
    BottomBorder: string
    LeftBorder: string
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

let parseSquare lines = 
    let top = List.head lines |> toString
    let bottom = List.last lines |> Seq.rev |> toString
    let left = lines |> List.map Seq.head |> Seq.rev |> toString
    let right = lines |> List.map Seq.last |> toString

    {
        TopBorder = top
        RightBorder = right
        BottomBorder = bottom
        LeftBorder = left
    }

let rec parse input tiles = 
    let input = input |> List.skipWhile (fun l -> l = "")
    match input with
    | [] -> tiles
    | h :: t -> printfn "Parsing square, line: %s" h
                let id = Regex.Match(h, "Tile (\\d+):").Groups.[1].Value
                let square = List.take 10 t
                             |> parseSquare
                parse (List.skip 10 t) ((id, square) :: tiles)

let squares = parse input List.empty

let next (x, y) = if x < 9
                  then (x + 1, y)
                  else (x, y + 1)

let arraySet (x, y) item array =
    let copy = Array2D.copy array
    copy.[x, y] <- item
    copy

let rotate rotation tile =
    match rotation with
    | 0 -> tile
    | 90 -> { TopBorder = tile.LeftBorder; RightBorder = tile.TopBorder; BottomBorder = tile.RightBorder; LeftBorder = tile.BottomBorder }
    | 180 -> { TopBorder = tile.BottomBorder; RightBorder = tile.LeftBorder; BottomBorder = tile.TopBorder; LeftBorder = tile.RightBorder }
    | 270 -> { TopBorder = tile.RightBorder; RightBorder = tile.BottomBorder; BottomBorder = tile.LeftBorder; LeftBorder = tile.TopBorder }
    | _ -> failwith "Invalid rotation"

let flip flip tile =
    match flip with
    | Original -> tile
    | Horizontal -> { TopBorder = reverse tile.TopBorder; RightBorder = reverse tile.LeftBorder; BottomBorder = reverse tile.BottomBorder; LeftBorder = reverse tile.RightBorder }
    | Vertical -> { TopBorder = reverse tile.BottomBorder; RightBorder = reverse tile.RightBorder; BottomBorder = reverse tile.TopBorder; LeftBorder = reverse tile.LeftBorder }
    | HorizontalVertical -> { TopBorder = tile.BottomBorder; RightBorder = tile.LeftBorder; BottomBorder = tile.TopBorder; LeftBorder = tile.RightBorder }

let transform transformation tile =
    tile |> rotate transformation.Rotation |> flip transformation.Flip

let border transformation tile border =
    let tile = tile |> transform transformation

    match border with
    | Top -> tile.TopBorder
    | Right -> tile.RightBorder
    | Bottom -> tile.BottomBorder
    | Left -> tile.LeftBorder

let square (x, y) image =
    if x >= 0 && x < Array2D.length1 image && y >= 0 && y < Array2D.length2 image
    then image.[x, y]
    else None

let fits image (tile, transformation) (x, y) =
    (match square (x, y - 1) image with None -> true | Some (above, aboveTransformation) -> (border transformation tile Top = border aboveTransformation above Bottom)) &&
    (match square (x + 1, y) image with None -> true | Some (right, rightTransformation) -> (border transformation tile Right = border rightTransformation right Left)) &&
    (match square (x, y + 1) image with None -> true | Some (below, belowTransformation) -> (border transformation tile Bottom = border belowTransformation below Top)) &&
    (match square (x - 1, y) image with None -> true | Some (left, leftTransformation) -> (border transformation tile Left = border leftTransformation left Right))

let allTransformations = [ 0; 90; 180; 270 ]
                         |> List.collect (fun rot -> [ { Rotation = rot; Flip = Original }
                                                       { Rotation = rot; Flip = Horizontal }
                                                       { Rotation = rot; Flip = Vertical } ])

let findSolution image (x, y) remainingTiles =
    if y >= Array2D.length2 image
    then [ image ]
    else remainingTiles
         |> List.collect (fun tile -> allTransformations |> List.map (fun transformation -> tile, transformation))
         |> List.filter (fun (tile, transformation) -> fits image (tile, transformation) (x, y))
         |> List.map (fun (tile, transformation) -> arraySet (x, y) (Some (tile, transformation)) image)



// let  

