open System
open System.IO

type State =
| Inactive
| Active

let input = File.ReadAllLines "17-conway-cubes-input.txt"

let space = Array3D.init
               input.[0].Length
               (Array.length input)
               1
               (fun x y z -> match input.[y].[x] with
                             | '.' -> Inactive
                             | '#' -> Active
                             | _ -> failwith "Invalid input")

let at (x, y, z) a =
    if x >= 0 && x < (Array3D.length1 a) && y >= 0 && y < (Array3D.length2 a) && z >= 0 && z < (Array3D.length3 a)
    then a.[x, y, z]
    else Inactive

let neighbors (x, y, z) =
    seq {
        for i in -1..1 do
            for j in -1..1 do
                for k in -1..1 do
                    if i <> 0 || j <> 0 || k <> 0
                    then yield x + i, y + j, z + k
    }

let evolvePosition (x, y, z) space =
    let activeNeighborCount = neighbors (x, y, z) |> Seq.filter (fun p -> at p space = Active) |> Seq.length
    match at (x, y, z) space with
    | Active -> match activeNeighborCount with
                | 2 | 3 -> Active
                | _ -> Inactive
    | Inactive -> match activeNeighborCount with
                  | 3 -> Active
                  | _ -> Inactive

let offset (x, y, z) o = (x + o, y + o, z + o)

let evolve space =
    Array3D.init
        ((Array3D.length1 space) + 2)
        ((Array3D.length2 space) + 2)
        ((Array3D.length3 space) + 2)
        (fun x y z -> evolvePosition (offset (x, y, z) -1) space)

let afterBoot = space |> evolve |> evolve |> evolve |> evolve |> evolve |> evolve

let result1 =
    seq {
        for x in 0..(Array3D.length1 afterBoot - 1) do
            for y in 0..(Array3D.length2 afterBoot - 1) do
                for z in 0..(Array3D.length3 afterBoot - 1) do
                    yield afterBoot.[x, y, z]
    }
    |> Seq.filter (fun s -> s = Active)
    |> Seq.length

// Part 2

let space2 = Array4D.init
                input.[0].Length
                (Array.length input)
                1
                1
                (fun x y z w -> match input.[y].[x] with
                                | '.' -> Inactive
                                | '#' -> Active
                                | _ -> failwith "Invalid input")

let at2 (x, y, z, w) a =
    if x >= 0 && x < (Array4D.length1 a) && y >= 0 && y < (Array4D.length2 a) && z >= 0 && z < (Array4D.length3 a) && w >= 0 && w < (Array4D.length4 a) 
    then a.[x, y, z, w]
    else Inactive

let neighbors2 (x, y, z, w) =
    seq {
        for i in -1..1 do
            for j in -1..1 do
                for k in -1..1 do
                    for l in -1..1 do
                        if i <> 0 || j <> 0 || k <> 0 || l <> 0
                        then yield x + i, y + j, z + k, w + l
    }

let evolvePosition2 (x, y, z, w) space =
    let activeNeighborCount = neighbors2 (x, y, z, w) |> Seq.filter (fun p -> at2 p space = Active) |> Seq.length
    match at2 (x, y, z, w) space with
    | Active -> match activeNeighborCount with
                | 2 | 3 -> Active
                | _ -> Inactive
    | Inactive -> match activeNeighborCount with
                  | 3 -> Active
                  | _ -> Inactive

let offset2 (x, y, z, w) o = (x + o, y + o, z + o, w + o)

let evolve2 space =
    Array4D.init
        ((Array4D.length1 space) + 2)
        ((Array4D.length2 space) + 2)
        ((Array4D.length3 space) + 2)
        ((Array4D.length4 space) + 2)
        (fun x y z w -> evolvePosition2 (offset2 (x, y, z, w) -1) space)

let afterBoot2 = space2 |> evolve2 |> evolve2 |> evolve2 |> evolve2 |> evolve2 |> evolve2

let result2 =
    seq {
        for x in 0..(Array4D.length1 afterBoot2 - 1) do
            for y in 0..(Array4D.length2 afterBoot2 - 1) do
                for z in 0..(Array4D.length3 afterBoot2 - 1) do
                    for w in 0..(Array4D.length4 afterBoot2 - 1) do
                        yield afterBoot2.[x, y, z, w]
    }
    |> Seq.filter (fun s -> s = Active)
    |> Seq.length