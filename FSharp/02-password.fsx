open System
open System.IO

let parse (line : string) =
    let segments = line.Split ' '
    let limits = segments.[0].Split '-'
    (limits.[0] |> Int32.Parse, limits.[1] |> Int32.Parse, (segments.[1].TrimEnd ':').[0], segments.[2])

let lines = File.ReadAllLines "02-password-input.txt"
            |> Array.map parse

let valid1 (min, max, ch, pwd) = 
    let cnt = pwd |> Seq.filter (fun c -> c = ch) |> Seq.length
    cnt >= min && cnt <= max

let result1 = lines
              |> Seq.filter valid1
              |> Seq.length

let valid2 (pos1, pos2, ch, (pwd : string)) = 
    (pwd.[pos1-1] = ch || pwd.[pos2-1] = ch) && not (pwd.[pos1-1] = ch && pwd.[pos2-1] = ch)

let result2 = lines
              |> Seq.filter valid2
              |> Seq.length