open Printf

let testStr = {|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|};;


let get_mul_ops s = 
        (* let s = Str.global_replace (Str.regexp "%%") "" s in *)
        let one_to_three_digits = {|\([0-9]\|[1-9][0-9]\|[1-9][1-9][0-9]\)|} in 
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
let () = printf "Sum:%d\n" (mul_and_sum (get_mul_ops testStr))


