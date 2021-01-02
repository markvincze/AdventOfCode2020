open System
open System.IO

type Deck = int list

let parse lines = 
    let cards1 = lines
                 |> Array.skip 1
                 |> Array.takeWhile (fun l -> l <> "")
                 |> Array.map Int32.Parse
                 |> List.ofArray
    
    let cards2 = lines
                 |> Array.skipWhile (fun l -> l <> "")
                 |> Array.skip 2
                 |> Array.map Int32.Parse
                 |> List.ofArray
    
    cards1, cards2

let deck1, deck2 = File.ReadAllLines "22-crab-combat-input.txt"
                   |> parse

let playRound deck1 deck2 =
    let h1 = List.head deck1
    let h2 = List.head deck2
    if h1 > h2
    then List.append (deck1 |> List.skip 1) [h1; h2], deck2 |> List.skip 1
    else deck1 |> List.skip 1, List.append (deck2 |> List.skip 1) [h2; h1]

let rec play deck1 deck2 =
    if List.isEmpty deck1
    then deck2
    else if List.isEmpty deck2
    then deck1
    else let deck1, deck2 = playRound deck1 deck2
         play deck1 deck2

let calculateScore deck =
    deck
    |> List.rev
    |> List.indexed
    |> List.sumBy (fun (i, card) -> (i + 1) * card)

let winnerDeck1 = play deck1 deck2
let result1 = winnerDeck1 |> calculateScore

// Part 2
type Player = Player1 | Player2 

let fingerprint deck1 deck2 =
    String.Join(",", deck1 |> List.map (fun c -> c |> string) |> Array.ofList) +
    "#" +
    String.Join(",", deck2 |> List.map (fun c -> c |> string) |> Array.ofList)

let rec recPlayRound deck1 deck2 =
    let h1 = List.head deck1
    let h2 = List.head deck2
    if (List.length deck1 > h1 && List.length deck2 > h2)
    then let winner, _ = recPlay Set.empty (deck1 |> List.skip 1 |> List.take h1) (deck2 |> List.skip 1 |> List.take h2)
         match winner with
         | Player1 -> List.append (deck1 |> List.skip 1) [h1; h2], deck2 |> List.skip 1
         | Player2 -> deck1 |> List.skip 1, List.append (deck2 |> List.skip 1) [h2; h1]
    else if h1 > h2
         then List.append (deck1 |> List.skip 1) [h1; h2], deck2 |> List.skip 1
         else deck1 |> List.skip 1, List.append (deck2 |> List.skip 1) [h2; h1]
and recPlay fingerprints deck1 deck2 =
    let print = fingerprint deck1 deck2
    if Set.contains print fingerprints
    then Player1, deck1
    else if List.isEmpty deck1
         then Player2, deck2
         else if List.isEmpty deck2
         then Player1, deck1
         else let deck1New, deck2New = recPlayRound deck1 deck2
              recPlay (Set.add (fingerprint deck1 deck2) fingerprints)  deck1New deck2New

let _, winnerDeck2 = recPlay Set.empty deck1 deck2
let result2 = winnerDeck2 |> calculateScore

