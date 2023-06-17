(* day01.ml *)

#use "topfind";;
#require "Str";;


let testf = "../data/day01-test.txt"
let inputf = "../data/day01-input.txt"

(** Read in all the lines from a file *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  contents
  |> Str.split (Str.regexp "\n\n")
  |> List.map (Str.split (Str.regexp "\n"))

(** Convert a nested list of strings to integers *)
let nested_string_to_int_list = List.map (fun x -> List.map int_of_string x)
let sum_list = List.fold_left (+) 0
let max_number_list = List.fold_left max min_int

let rec take k xs =
  match xs with
  | [] -> failwith "take"
  | x :: xs -> if k=1 then [x] else x :: take (k-1) xs

let part1 file =
  read_lines file
  |> nested_string_to_int_list
  |> List.map sum_list
  |> max_number_list

let part2 file =
  read_lines file
  |> nested_string_to_int_list
  |> List.map sum_list
  |> List.sort compare
  |> List.rev
  |> take 3
  |> sum_list

(* The End *)
