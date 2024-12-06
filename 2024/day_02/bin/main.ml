open Stdlib
open Printf

let () = print_endline "Hello, World!";;

let testStr =
{|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|};;

let make_reports str : int list list=
        let lines = String.split_on_char '\n' str in
        let nums_f l = List.map (fun c -> int_of_string c) l in
        List.map (fun l -> nums_f (String.split_on_char ' ' l)) lines;;


let report_is_safe report : bool =
        let f a b = if (List.hd report) < (List.hd (List.tl report)) then (a < b && a+3 >= b) else (a > b && a-3 <= b) in
        let rec pairs r = 
                match r with
                | x :: y :: _ -> if f x y then pairs (List.tl r) else false
                | _ -> true
        in
        pairs report;;

let count_safe reports = 
        List.length (List.filter (fun r -> report_is_safe r) reports);;


(* print test str *)
let () = List.iter (fun x ->  List.iter (fun y -> printf "%d;" y) x; printf "\n") (make_reports testStr);;
let () = printf "%B\n" (report_is_safe (List.hd (List.tl (make_reports testStr))));;
let () = printf "Safe: %d\n" (count_safe (make_reports testStr));;


let read_file_to_strings filename =
        let file = In_channel.open_text filename in
        let strings = In_channel.input_lines file in
        In_channel.close file; (List.fold_left (fun init x -> init ^ "\n" ^ x) (List.hd strings) (List.tl strings));;

let () = printf "Safe: %d\n" (count_safe (make_reports (read_file_to_strings "data-p1.txt")));;

let remove_1 l ind =
        List.filteri (fun i _ -> ind <> i) l;;

let report_is_safe_one_fault report : bool =
        let res = report_is_safe report in
        match res with
        | true -> true
        | false -> not (List.is_empty (List.filteri (fun i _ -> (report_is_safe (remove_1 report i))) report))


let count_safe_one_fault reports = 
        List.length (List.filter (fun r -> report_is_safe_one_fault r) reports);;

let () = printf "Safe test Fault tolerant: %d\n" (count_safe_one_fault (make_reports testStr));;
let () = printf "Safe line test Fault tolerant: %d\n" (count_safe_one_fault (make_reports "1 2 7 8 9"));;
let () = printf "Safe test Fault tolerant: %d\n" (count_safe_one_fault (make_reports testStr));;
let () = printf "Safe Fault tolerant: %d\n" (count_safe_one_fault (make_reports (read_file_to_strings "data-p1.txt")));;
