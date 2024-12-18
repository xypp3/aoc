open Printf
let () = print_endline "Hello, World!";;

let file_to_string filename =
        In_channel.(with_open_text filename input_all)

let make_grid str =
        let lines = String.split_on_char '\n' str in
        List.map (fun l -> List.init (String.length l) (String.get l)) lines;;

let get_char_coords grid chr =
        let maybe_coords = List.mapi (fun i l -> List.mapi (fun j c -> match chr = c with
                | true -> (i,j)
                | false -> (-1,-1)
        ) l) grid in
        List.filter (fun x -> match x with
                | (-1,-1) -> false
                | _ -> true
        ) (List.flatten maybe_coords);;

let big = file_to_string "big.txt";;
big
let small_1 = 
{|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|};;
small_1

let () = List.iter (fun (x,y) -> printf "%d,%d\n" y x) (get_char_coords (make_grid small_1) 'X')

let all_dirs = [(1,0);
        (1,1);
        (0,1);
        (-1,1);
        (-1,0);
        (-1,-1);
        (0,-1);
        (1,-1);];;
all_dirs

let scale_coord coord scale =
        let y,x = coord in
        y*scale, x*scale;;

let add_coord c1 c2 =
        let y1,x1 = c1 in
        let y2,x2 = c2 in
        (y1+y2, x1+x2);;

let coord_in_grid coord both_lens =
        let y,x = coord in
        let y_len, x_len = both_lens in
        0 <= x && x < x_len && 0 <= y && y < y_len

let grid_len grid = 
        let y_len = List.length grid in
        let x_len = List.length (List.nth grid 0) in
        (y_len, x_len)

let char_at_coord grid coord =
        let y,x = coord in
        try List.nth (List.nth grid y) x
        with Failure _ -> printf "%dhiii%d\n" y x; '~'

let is_word_in_dir grid start_c dir_c =
        let magic_word = ['X';'M';'A';'S'] in
        let len = grid_len grid in
        let all_coords = List.mapi (fun i _ -> add_coord start_c (scale_coord dir_c (i))) magic_word in
        (* let () = List.iter (fun (x,y) -> print_endline ((string_of_int y)^(string_of_int x))) all_coords in *)

        let filter_char coord chr = 
                (coord_in_grid coord len) && ((char_at_coord grid coord) = chr) 
        in

        let rec check_magic_word coords word =
                match coords with
                | [] -> true
                | x :: xs -> match filter_char x (List.hd word) with
                        | true -> check_magic_word xs (List.tl word)
                        | false -> false
        in
        check_magic_word all_coords magic_word;;

let count_xmas str =
        let grid = make_grid str in
        let all_x = (get_char_coords grid 'X') in
        let results = List.flatten (List.map (fun x -> List.map (fun dir -> is_word_in_dir grid x dir) all_dirs) all_x) in
        List.fold_left (fun acc a -> if a = true then acc + 1 else acc) 0 results

let () = Printf.printf "%d\n" (count_xmas small_1)
let () = Printf.printf "%d\n" (count_xmas big)

(* let mas_in_x grid start_c dir_c = *)
(*         let magic_word = ['X';'M';'A';'S'] in *)
(*         let len = grid_len grid in *)
(*         let all_coords = List.mapi (fun i _ -> add_coord start_c (scale_coord dir_c (i))) magic_word in *)
(*         (* let () = List.iter (fun (x,y) -> print_endline ((string_of_int y)^(string_of_int x))) all_coords in *) *)
(**)
(*         let filter_char coord chr =  *)
(*                 (coord_in_grid coord len) && ((char_at_coord grid coord) = chr)  *)
(*         in *)
(**)
(*         let rec check_magic_word coords word = *)
(*                 match coords with *)
(*                 | [] -> true *)
(*                 | x :: xs -> match filter_char x (List.hd word) with *)
(*                         | true -> check_magic_word xs (List.tl word) *)
(*                         | false -> false *)
(*         in *)
(*         check_magic_word all_coords magic_word;; *)
(**)
(* let count_mas_x str = *)
(*         let grid = make_grid str in *)
(*         let all_x = (get_char_coords grid 'X') in *)
(*         let results = List.flatten (List.map (fun x -> List.map (fun dir -> is_word_in_dir grid x dir) all_dirs) all_x) in *)
(*         List.fold_left (fun acc a -> if a = true then acc + 1 else acc) 0 results;; *)
