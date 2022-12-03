open System ;;
open System.IO ;;
open System.Text.RegularExpressions ;;

type Play = Rock | Paper | Scissors ;;
type Outcome = PWin | OWin | Draw ;;

let playOutcome (opponent, player) =
    match (player, opponent) with
    | Rock, Paper     -> OWin
    | Rock, Scissors  -> PWin
    | Paper, Rock     -> PWin
    | Paper, Scissors -> OWin
    | Scissors, Rock  -> OWin
    | Scissors, Paper -> PWin
    | _               -> Draw
;;

// almost sure there some standard function for this a la Haskell
let tupleLiftOption (x, y) =
    match (x, y) with
    | (Some x, Some y) -> Some (x, y)
    | _                -> None
;;

let parsePlay s = if (String.IsNullOrWhiteSpace(s)) then None
                    else let c = s.ToCharArray()[0] in 
                          match c with
                          | 'A' -> Some Rock
                          | 'X' -> Some Rock
                          | 'B' -> Some Paper
                          | 'Y' -> Some Paper
                          | 'C' -> Some Scissors
                          | 'Z' -> Some Scissors
                          | _ -> None
;;

let scoreStrategy play =
    match play with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
;;

let scoreOutcome outcome = 
    match outcome with
    | PWin -> 6
    | Draw -> 3
    | OWin -> 0

let scorePlay (p: (Play * Play) option) =
    match p with
    | Some (opponent, player) -> (scoreStrategy player) + (playOutcome (opponent, player) |> scoreOutcome)
    | _ -> 0
;;

File.ReadLines("input.txt") 
// you may say this decomposition into a map and a fold is kind of arbitrary - surely you can do it all in
// a single fold! Pretty sure yes, but this is where things landed as bedtime on the 2nd approached :)
    |> Seq.map (fun lin -> let m = Regex.Match(lin, @"(\w)\s*(\w)")
                            in if (m.Success) 
                                then tupleLiftOption(parsePlay m.Groups[1].Value, parsePlay m.Groups[2].Value) 
                                else None 
                )
    |> Seq.fold (fun acc key -> acc + (scorePlay key)) 0
    |> printf "%A\n"
;;