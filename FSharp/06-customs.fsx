open System.IO

let lines = File.ReadAllLines "06-customs-input.txt"
            |> List.ofArray

let rec sumCountsAnyone lines yesAnswers sum =
    match lines with
    | [] -> sum + (Set.count yesAnswers)
    | h :: t -> if h = ""
                then sumCountsAnyone t Set.empty<char> (sum + (Set.count yesAnswers))
                else sumCountsAnyone t (Set.union yesAnswers (Set.ofSeq h)) sum

let result1 = sumCountsAnyone lines Set.empty<char> 0

let allChars = [ 'a'..'z' ] |> Set.ofList

let rec sumCountsEveryone lines noAnswers sum =
    match lines with
    | [] -> sum + (Set.count (Set.difference allChars noAnswers))
    | h :: t -> if h = ""
                then sumCountsEveryone t Set.empty<char> (sum + (Set.count (Set.difference allChars noAnswers)))
                else sumCountsEveryone t (Set.union noAnswers (Set.difference allChars (Set.ofSeq h))) sum

let result2 = sumCountsEveryone lines Set.empty<char> 0