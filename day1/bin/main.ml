open Stdlib
open Printf

(*
let print_list (input: 'a list) =
    let rec inner acc =
        match acc with
        | [] -> ()
        | [ x ] -> print_string x
        | head :: tail -> 
            print_string (head ^ ", ");
            inner tail
    in
    inner input
*)
let get_parts () =
    let rec inner lists acc =
        match In_channel.input_line stdin with
        | Some "" -> inner (acc :: lists) []
        | Some x -> inner lists (x :: acc)
        | None -> 
            match acc with
            | [] -> lists
            | acc -> (acc :: lists)
    in
    inner [] []
    |> List.rev

let get_sums input =
    input
    |> List.map (int_of_string)
    |> List.fold_left (+) 0

let _part_one (sums: int list) =
    List.fold_left (max) 0 sums

let _part_two (sums: int list) =
    let sorted = List.sort (-) sums in
    sorted
    |> List.rev
    |> List.to_seq
    |> Seq.take 3
    |> Seq.fold_left (+) 0

let () = 
    let sums =
        get_parts() 
        |> List.map get_sums
    in
    printf "Part 1: %d\n" (_part_one sums);
    printf "Part 2: %d\n" (_part_two sums);
