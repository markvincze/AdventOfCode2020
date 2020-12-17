open System
open System.IO

type Field = {
    Name : string
    Ranges: (int * int) list
}

type Input = {
    Fields : Field list
    YourTicket : int list
    NearbyTickets : (int list) list
}

let parseField (line : string) = 
    let segments = line.Split(": ")
    let ranges = segments.[1].Split(" or ")

    {
        Name = segments.[0]
        Ranges = ranges
                 |> Array.map (fun r -> let nums = r.Split('-')
                                        (Int32.Parse nums.[0], Int32.Parse nums.[1]))
                 |> List.ofArray
    }

let parseTicket (line : string) = line.Split(',') |> Array.map Int32.Parse |> List.ofArray

type ParseSection = | Fields | YourTicket | NearbyTickets

let rec parse parseSection fields yourTicket nearbyTickets lines =
    match lines with
    | [] -> { Fields = fields; YourTicket = yourTicket; NearbyTickets = nearbyTickets }
    | h :: t -> match parseSection with
                | Fields -> match h with
                            | "" -> parse YourTicket fields yourTicket nearbyTickets (t |> List.skip 1)
                            | h -> parse Fields (parseField h :: fields) yourTicket nearbyTickets t
                | YourTicket -> parse NearbyTickets fields (parseTicket h) nearbyTickets (t |> List.skip 2)
                | NearbyTickets -> parse NearbyTickets fields yourTicket ((parseTicket h) :: nearbyTickets) t

let input = File.ReadAllLines "16-ticket-input.txt"
            |> List.ofArray
            |> parse Fields [] [] []

let doesMatch field n =
    field.Ranges
    |> List.exists (fun (min, max) -> n >= min && n <= max)

let result1 = input.NearbyTickets
              |> List.collect id
              |> List.filter (fun v -> input.Fields |> List.forall (fun field -> not (doesMatch field v)))
              |> List.sum

let fieldCount = input.Fields |> List.length

let canFieldBeNth field input validNearbyTickets n =
    input.YourTicket :: validNearbyTickets
    |> List.forall (fun ticket -> doesMatch field (ticket |> List.item n))

let rec findPairings input validNearbyTickets unassignedFields (pairings : Map<Field, int>) =
    match unassignedFields with
    | [] -> pairings
    | unassignedFields ->
        let coveredFieldIndexes = pairings |> Seq.map (fun kvp -> kvp.Value) |> List.ofSeq
        let unassignedFieldIndexes = [ 0..(fieldCount - 1) ]
                                     |> List.except coveredFieldIndexes

        let possiblePairings =
            unassignedFields
            |> List.map (fun f -> f, unassignedFieldIndexes |> List.filter (fun n -> canFieldBeNth f input validNearbyTickets n))

        let nextField, [ nextFieldIndex ] =
            possiblePairings
            |> List.find (fun (_, ns) -> ns |> List.length = 1)

        findPairings input validNearbyTickets  (unassignedFields |> List.except [nextField]) (Map.add nextField nextFieldIndex pairings)

let validNearbyTickets =
    input.NearbyTickets
    |> List.filter (fun ticket -> ticket |> List.forall (fun value -> input.Fields |> List.exists (fun field -> doesMatch field value)))

let allTicketCount = input.NearbyTickets |> List.length
let validNearbyTicketCount = validNearbyTickets |> List.length

let pairings = findPairings input validNearbyTickets input.Fields Map.empty<Field, int>

let result2 = pairings
              |> Seq.filter (fun kvp -> kvp.Key.Name.StartsWith "departure")
              |> Seq.fold (fun p kvp -> p * (int64 (input.YourTicket |> List.item kvp.Value))) 1L
