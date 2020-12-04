open System.IO

type Passport = {
    BirthYear : string option
    IssueYear : string option
    ExpirationYear : string option
    Height : string option
    HairColor : string option
    EyeColor : string option
    PassportId : string option
    CountryId : string option
}

let emptyPassport = { BirthYear = None; IssueYear = None; ExpirationYear = None; Height = None; HairColor = None; EyeColor = None; PassportId = None; CountryId = None; }

let isValid passport =
    passport.BirthYear.IsSome && passport.IssueYear.IsSome && passport.ExpirationYear.IsSome && passport.Height.IsSome && 
    passport.HairColor.IsSome && passport.EyeColor.IsSome && passport.PassportId.IsSome

let lines = File.ReadAllLines "04-passport-input.txt"
            |> List.ofArray

let addProp passport (propStr : string) =
    let split = propStr.Split(':')
    match split.[0] with
    | "byr" -> { passport with BirthYear = Some split.[1]}
    | "iyr" -> { passport with IssueYear = Some split.[1]}
    | "eyr" -> { passport with ExpirationYear = Some split.[1]}
    | "hgt" -> { passport with Height = Some split.[1]}
    | "hcl" -> { passport with HairColor = Some split.[1]}
    | "ecl" -> { passport with EyeColor = Some split.[1]}
    | "pid" -> { passport with PassportId = Some split.[1]}
    | "cid" -> { passport with CountryId = Some split.[1]}
    | _ -> failwith "Invalid input"

let rec collectPassports lines passports passport =
    match lines with
    | [] -> (passport :: passports)
    | h :: t -> match h with
                | "" -> collectPassports t (passport :: passports) emptyPassport
                | h -> collectPassports t passports (h.Split(' ') |> Array.fold addProp passport)

let result1 =
    collectPassports lines [] emptyPassport
    |> List.filter isValid
    |> List.length