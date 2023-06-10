(* day01.ml *)

let testf = "data/day01-test.txt"
let inputf = "data/day01-input.txt"

(** Read in all the lines from a file *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

(** Split and group an array separated on empty strings *)
let split_by_empty_string lst =
  let add_to_group acc x =
    if x = "" then [] :: acc
    else match acc with
         | [] -> [[x]]
         | (h :: t) -> (x :: h) :: t
  in
  List.fold_left add_to_group [[]] lst |> List.rev |> List.filter (fun x -> x <> [])

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
  |> split_by_empty_string
  |> nested_string_to_int_list
  |> List.map sum_list
  |> max_number_list

let part2 file =
  read_lines file
  |> split_by_empty_string
  |> nested_string_to_int_list
  |> List.map sum_list
  |> List.sort compare
  |> List.rev
  |> take 3
  |> sum_list

(* The End *)
