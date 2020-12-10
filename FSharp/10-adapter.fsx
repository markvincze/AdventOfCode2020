open System
open System.IO

let input = File.ReadAllLines "10-adapter-input.txt"
            |> Array.map Int32.Parse

Array.sortInPlace input

let rec countDifferences (input : int array) previousValue index diff1 diff3 =
    if index >= Array.length input
    then (diff1), (diff3 + 1)
    else match input.[index] - previousValue with
         | 1 -> countDifferences input input.[index] (index + 1) (diff1 + 1) diff3
         | 2 -> countDifferences input input.[index] (index + 1) diff1 diff3
         | 3 -> countDifferences input input.[index] (index + 1) diff1 (diff3 + 1)
         | _ -> failwith "Invalid difference"

let diff1, diff3 = countDifferences input 0 0 0 0

let result1 = diff1 * diff3