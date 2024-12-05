open Stdlib

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
        List.map (fun l -> nums_f (String.split_on_char ' ' l) ) lines;;


(* print test str *)
let () = List.iter (fun x ->  List.iter (fun y -> Printf.printf "%d;" y) x; Printf.printf "\n") (make_reports testStr);;

