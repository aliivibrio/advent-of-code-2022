open System ;;
open System.IO ;;

// active pattern to use in assigning priority to items
let (|Upper|Lower|Invalid|) ch = if Char.IsUpper ch then Upper
                                    else if Char.IsLower ch then Lower
                                        else Invalid ;;

// Item definition and priority function
type Item = ItemCode of char ;;

let priority (ItemCode code)  = 
    let enc = Convert.ToInt32 code in
    match code with
    | Upper c   -> enc - 38
    | Lower c   -> enc - 96
    | _         -> 0
;;

// Rucksack definition and functions
type Rucksack = Compartments of (Item Set * Item Set) ;;

let inBothCompartments (Compartments(left, right)) = Set.intersect left right ;;

let allContents (Compartments(left, right)) = Set.union left right ;;

let parseCompartment (str:string) = str.ToCharArray() |> Seq.map ItemCode |> Set ;;

let parseRucksack (str:string) = let mid            = str.Length / 2
                                 let leftContents   = str.Substring(0, mid)     |> parseCompartment
                                 let rightContents  = str.Substring(mid, mid)   |> parseCompartment
                                  in Compartments (leftContents, rightContents)
;;

// do the thing
File.ReadLines("input.txt")
    |> Seq.map parseCompartment
    |> Seq.toList
    |> List.chunkBySize 3
    |> List.sumBy (Set.intersectMany >> Set.toList >> List.sumBy priority)
    |> printf "%A\n"
;;