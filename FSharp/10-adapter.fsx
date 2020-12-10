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

let ways = Array.zeroCreate<int64> (Array.length input)

let rec countPossibleWays (input : int array) (ways : int64 array) index =
    let al = Array.length input
    if index = al - 1
    then ways.[index] <- 1L
         countPossibleWays input ways (index - 1)
    else let current = if index < 0 then 0 else input.[index]
         let step1 = ways.[index + 1]
         let step2 = if (index + 2 < al && input.[index + 2] - current <= 3) then ways.[index + 2] else 0L
         let step3 = if (index + 3 < al && input.[index + 3] - current <= 3) then ways.[index + 3] else 0L
         if index < 0
         then step1 + step2 + step3
         else ways.[index] <- step1 + step2 + step3
              countPossibleWays input ways (index - 1)

let result2 = countPossibleWays input (Array.zeroCreate<int64> (Array.length input)) (Array.length input - 1)