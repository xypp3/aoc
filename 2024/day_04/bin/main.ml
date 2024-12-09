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

let () = List.iter (fun (x,y) -> printf "%d,%d\n" x y) (get_char_coords (make_grid small_1) 'X')
