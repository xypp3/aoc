open Printf

let testStr = {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|};;


let get_mul_ops s = 
        (* let s = Str.global_replace (Str.regexp "%%") "" s in *)
        let one_to_three_digits = {|\([0-9]\|[0-9][0-9]\|[0-9][0-9][0-9]\)|} in 
        let regexp = Str.regexp ("mul(" ^ one_to_three_digits ^ "," ^ one_to_three_digits ^ ")") in
        let len = String.length s in
        let int_of match_g = Stdlib.int_of_string (Str.matched_group match_g s) in
        let rec findall i l =
        match (Str.string_match regexp s i), i with
        | true, _ -> findall (i+1) (l @ [(int_of 1) , (int_of 2)])
        | false, x when x < len  -> findall (i+1) l
        | false, _ -> l
        in
        findall 0 [];;

let mul_and_sum mul_ops =
        List.fold_left (+) 0 (List.map (fun (x,y) -> x*y) mul_ops)

(* let () = printf "%d\n" (List.length (get_mul_ops "mul(111,1)mul(0,0)mul[1,2]")) *)
(* let () = List.iter (fun (x,y) -> printf "%d %d\n" x y) (get_mul_ops "mul(111,1)mul(0,0)mul[1,2]") *)
let () = List.iter (fun (x,y) -> printf "%d %d\n" x y) (get_mul_ops testStr)
(* let () = List.iter (fun (x,y) -> printf "%d %d\n" x y) (get_mul_ops testStr) *)
let () = printf "Sum Here:%d\n" (mul_and_sum (get_mul_ops testStr))

let read_file_to_strings filename =
        let file = In_channel.open_text filename in
        let strings = In_channel.input_lines file in
        In_channel.close file; (List.fold_left (fun init x -> init ^ "\n" ^ x) (List.hd strings) (List.tl strings));;

let () = printf "Sum big:%d\n" (mul_and_sum (get_mul_ops (read_file_to_strings "big.txt")))


let testStr2 = {|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|}
type enable = 
        | DOe
        | DONTe;;
type op =
        | DO
        | DONT
        | MUL
        | FIN;;

let get_mul_ops_part_2 s = 
        (* let s = Str.global_replace (Str.regexp "%%") "" s in *)
        let one_to_three_digits = {|\([0-9]\|[0-9][0-9]\|[0-9][0-9][0-9]\)|} in 
        let regexp = Str.regexp ("mul(" ^ one_to_three_digits ^ "," ^ one_to_three_digits ^ ")") in
        let re_do = Str.regexp "do()" in
        let re_dont = Str.regexp "don't()" in


        let search_mul i = try Some (Str.search_forward regexp s i) with Not_found -> None in
        let search_do i = try Some (Str.search_forward re_do s i) with Not_found -> None in
        let search_dont i = try Some (Str.search_forward re_dont s i) with Not_found -> None in

        let f a b = 
        match b with
                | Some x -> a <= x
                | None -> true
        in
        let search i = 
        match search_do i, search_dont i, search_mul i with
                | y,z,Some x when f x y && f x z ->  (MUL, x)
                | Some y,z, x when f y x && f y z -> (DO, y)
                | y, Some z, x when f z y && f z x ->  (DONT, z)
                | _,_, None -> (FIN, 0)
                | _,_,_ -> (FIN, 0)
        in


        let int_of match_g = Stdlib.int_of_string (Str.matched_group match_g s) in
        let rec findall i l e =
        match search i, e with
        | (MUL,j), DOe ->  findall (j+1) (l @ [(int_of 1) , (int_of 2)]) e
        | (MUL,j), DONTe ->  findall (j+1) l e
        | (DO, j), _ -> findall (j+1) l DOe
        | (DONT, j), _ -> findall (j+1) l DONTe
        | (FIN, _), _ -> l
        in
        findall 0 [] DOe;;

get_mul_ops_part_2 testStr2;;
let () = printf "Sum test: %d\n" (mul_and_sum (get_mul_ops_part_2 testStr2))
let () = printf "Sum big part 2: %d\n" (mul_and_sum (get_mul_ops_part_2 (read_file_to_strings "big.txt")))
