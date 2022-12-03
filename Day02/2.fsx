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

let playForOutcome (opponent, outcome) =
    match (opponent, outcome) with
    | Rock, OWin -> Scissors
    | Rock, PWin -> Paper
    | Paper, OWin -> Rock
    | Paper, PWin -> Scissors
    | Scissors, OWin -> Paper
    | Scissors, PWin -> Rock
    | (any, Draw) -> any
;;
let tupleLiftOption (x, y) =
    match (x, y) with
    | (Some x, Some y) -> Some (x, y)
    | _                -> None
;;

let parsePlay s = if (String.IsNullOrWhiteSpace(s)) then None
                    else let c = s.ToCharArray()[0] in 
                          match c with
                          | 'A' -> Some Rock
                          | 'B' -> Some Paper
                          | 'C' -> Some Scissors
                          | _ -> None
;;

let parseOutcome s = if (String.IsNullOrWhiteSpace(s)) then None
                       else let c = s.ToCharArray()[0] 
                            in 
                            match c with
                            | 'X' -> Some OWin
                            | 'Y' -> Some Draw
                            | 'Z' -> Some PWin
                            | _ -> None
;;
let scoreStrategy play = match play with
                            | Rock -> 1
                            | Paper -> 2
                            | Scissors -> 3
;;

let scoreOutcome outcome = 
    match outcome with
    | PWin -> 6
    | Draw -> 3
    | OWin -> 0

let scorePlay (p: (Play * Outcome) option) =
    match p with
    | Some (opponent, outcome) -> (playForOutcome (opponent, outcome) |> scoreStrategy) + scoreOutcome outcome
    | _ -> 0
;;

File.ReadLines("input.txt") 
    |> Seq.map (fun lin -> let m = Regex.Match(lin, @"(\w)\s*(\w)")
                            in if (m.Success) 
                                then tupleLiftOption(parsePlay m.Groups[1].Value, parseOutcome m.Groups[2].Value) 
                                else None 
                )
    |> Seq.fold (fun acc key -> acc + (scorePlay key)) 0
    |> printf "%A\n"
;;