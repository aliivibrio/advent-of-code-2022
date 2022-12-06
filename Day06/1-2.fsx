open System ;;
open System.IO ;;

type State = Found of int | StillSearching of (int * char list) ;;

let updateState (n: int) (s:State) (c:char) : State =
    match s with
    | Found _                        -> s
    | StillSearching (idx, currList) ->
        match List.tryFindIndex ((=) c) currList with
        | None      -> if List.length currList + 1 = n 
                        then Found idx 
                        else StillSearching (idx+1, currList @ [c])
        | Some prev -> let (drop, keep) = (List.splitAt (prev+1) currList) in 
                       StillSearching (idx+1, keep @ [c])
;;

let markerLength = match Array.tryItem 1 fsi.CommandLineArgs with
                   | None   -> printf "No marker length provided on command line, defaulting to 4\n"
                               4
                   | Some a -> Int32.Parse(a) ;;

File.ReadLines("input.txt") 
    |> Seq.toList 
    |> List.map (fun sample ->
                    sample.ToCharArray() 
                    |> Array.toList 
                    |> List.fold (updateState markerLength) (StillSearching (1, [])) 
                    |> printf "%A\n"
                )
;;