open System.IO
open System
open System.Text.RegularExpressions

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

let isValid1 passport =
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
    |> List.filter isValid1
    |> List.length

let isValidYear min max str =
    match str with
    | None -> false
    | Some str -> if String.length str <> 4
                  then false
                  else match Int32.TryParse str with
                       | true, y -> y >= min && y <= max
                       | false, _ -> false

let isValidHeight str =
    match str with
    | None -> false
    | Some str -> let rMatch = Regex.Match (str, @"^(\d+)(cm|in)$")
                  if rMatch.Success
                  then let height = rMatch.Groups.[1].Value |> Int32.Parse
                       match rMatch.Groups.[2].Value with
                       | "cm" -> height >= 150 && height <= 193
                       | "in" -> height >= 59 && height <= 76
                       | _ -> false
                  else false

let isRegexMatch reg str =
    match str with
    | None -> false
    | Some str -> (Regex.Match (str, reg)).Success

let isValid2 passport =
    passport.BirthYear |> isValidYear 1920 2002 &&
    passport.IssueYear |> isValidYear 2010 2020 &&
    passport.ExpirationYear |> isValidYear 2020 2030 &&
    passport.Height |> isValidHeight &&
    passport.HairColor |> isRegexMatch @"^#[\da-f]{6}$" &&
    passport.EyeColor |> isRegexMatch @"^(amb|blu|brn|gry|grn|hzl|oth)$" &&
    passport.PassportId |> isRegexMatch @"^\d{9}$"

let result2 =
    collectPassports lines [] emptyPassport
    |> List.filter isValid2
    |> List.length