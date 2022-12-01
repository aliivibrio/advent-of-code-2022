open System ;;
open System.IO ;;

let folder = fun acc key ->
    let (isInt, num) = if (key = String.Empty) then (false, 0) else Int32.TryParse(key)
        in
    match acc with
    | []    when isInt      -> [num] 
    | []                    -> [] 
    | l::ls when isInt      -> (l+num)::ls
    | l::ls                 -> 0::acc
;;

File.ReadLines("input.txt") 
    |> Seq.fold folder []  
    |> List.max
    |> printf "%d\n"
;;