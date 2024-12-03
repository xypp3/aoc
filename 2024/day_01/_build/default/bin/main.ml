open Stdlib
open String

let () = print_endline "Hello, World!";;

let test_pt1 = {|3   4
4   3
2   5
1   3
3   9
3   3|}

let first_n_last str = 
        let first_end = index str ' ' in
        let second_start = rindex str ' ' in
        let first = int_of_string(sub str 0 first_end) in
        let second = int_of_string(sub str (second_start+1) ((length str) - second_start - 1)) in
        first, second;;

let split_in_middle str : int = 
        let lines = split_on_char '\n' str in
        let rec split l = 
                match l with
                | [] -> [], []
                | x :: xs -> 
                        let f,s = first_n_last x in
                        let r_firs, r_sec = split xs in
                        (r_firs @ [f], r_sec @ [s])
        in
        let f, s = split lines in
        let f_sort = List.sort Int.compare f in
        let s_sort = List.sort Int.compare s in
        List.fold_left (+) 0 (List.map2 (fun a b -> Int.abs (a-b)) s_sort f_sort);;


let () = let x, s = (first_n_last "123    456") in
        Printf.printf "%d,%d\n" x s

let () = Printf.printf "Ans:%d\n" (split_in_middle test_pt1);;

let read_file_to_strings filename =
  let file = In_channel.open_text filename in
  let strings = In_channel.input_lines file in
  In_channel.close file;
  (List.fold_left (fun init x -> init ^ "\n" ^ x) (List.hd strings) (List.tl strings))

let () = Printf.printf "Ans:%d\n" (split_in_middle (read_file_to_strings "part1.txt"));;

let multiply_left_by_count_right str : int = 
        let lines = split_on_char '\n' str in
        let rec split l = 
                match l with
                | [] -> [], []
                | x :: xs -> 
                        let f,s = first_n_last x in
                        let r_firs, r_sec = split xs in
                        (r_firs @ [f], r_sec @ [s])
        in
        let f, s = split lines in
        List.fold_left (+) 0 (List.map (fun a -> a * (List.length (List.filter (fun b -> Int.equal a b) s))) f);;

let () = Printf.printf "Ans:pt2-test:%d\n" (multiply_left_by_count_right test_pt1);;
let () = Printf.printf "Ans:pt2:%d\n" (multiply_left_by_count_right (read_file_to_strings "part1.txt"));;

