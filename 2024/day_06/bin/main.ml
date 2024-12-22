open Printf

let file_to_string filename =
        In_channel.(with_open_text filename input_all)

let txt_big = file_to_string "input_big.txt";;
txt_big
let txt_small_1 = {|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|};;
txt_small_1


let make_grid str =
        Array.map (fun l -> (Array.init (String.length l) (String.get l))) (Array.of_list (String.split_on_char '\n' str))

let max_x_grid grid =
        Array.length grid.(0)
let max_y_grid grid =
        (Array.length grid)


let find_char grid char =
        Array.to_list grid
        |> List.mapi (fun i l -> 
                Array.to_list l
                |> List.mapi (fun j c -> (c,i,j))
                |> List.filter (fun (c,_,_) -> c=char)
                |> List.map (fun (_,i,j) -> (i,j)))
        |> List.flatten


let walk_to_end grid =
        let start = List.nth (find_char grid '^') 0 in
        let tmpy, tmpx = start in
        grid.(tmpy).(tmpx) <- 'X';
        let dir = (-1,0) in
        let max_x = max_x_grid grid in
        let max_y = max_y_grid grid in
        let next_dir dir = match dir with
                | (-1,0) -> (0,1)
                | (0,1) -> (1,0)
                | (1,0) -> (0,-1)
                | (0,-1) -> (-1,0)
                | _ -> printf "direction doesn't exist"; (0,0)
        in
        let can_walk_in start dir =
                let sy, sx = start in
                let dy, dx = dir in
                let y, x = (sy+dy),(sx+dx) in
                if 0 <= y && y < (max_y) && 0 <= x && x < max_x then
                        try let _ = grid.(y).(x) in Some (y,x) with
                        | Invalid_argument _ -> printf "invalid pos"; None
                else
                        None
        in
        let rec walk_in start dir =
                match can_walk_in start dir with
                | Some (y,x) when grid.(y).(x) <> '#' -> grid.(y).(x) <- 'X'; walk_in (y,x) dir
                | Some (y,x) when grid.(y).(x) = '#' -> Some start
                | Some _ -> printf "error when walking in line"; Some (420,420)
                | None -> None
        in
        let rec walk start dir = 
                match walk_in start dir with
                | Some pos -> walk pos (next_dir dir)
                | None -> ()
        in
        walk start dir;;
let () =
        (* let grid = make_grid txt_small_1 in *)
        (* Array.iteri (fun i l -> printf "-"; Array.iteri (fun j _ -> printf "%c" grid.(i).(j)) l; printf "\n") grid; *)
        (* printf "\n\n"; *)
        (* walk_to_end grid; *)
        (* Array.iteri (fun i l -> printf "-"; Array.iteri (fun j _ -> printf "%c" grid.(i).(j)) l; printf "\n") grid; *)
        (* printf "Ans-small:%d\n" (List.length (find_char grid 'X')); *)
        printf "we starting\n";
        let grid = make_grid txt_big in
        (* Array.iteri (fun i l -> printf "-"; Array.iteri (fun j _ -> printf "%c" grid.(i).(j)) l; printf "\n") grid; *)
        walk_to_end grid;
        (* Array.iteri (fun i l -> Array.iteri (fun j _ -> printf "%c" grid.(i).(j)) l; printf "\n") grid; *)
        (* List.iter (fun (y,x) -> printf "%d,%d\n" y x) (find_char grid 'X'); *)
        printf "Ans-big:%d\n" (List.length (find_char grid 'X'));;

let detect_loop grid =
        let start = List.nth (find_char grid '^') 0 in
        let tmpy, tmpx = start in
        grid.(tmpy).(tmpx) <- 'X';
        let dir = (-1,0) in
        let max_x = max_x_grid grid in
        let max_y = max_y_grid grid in

        let next_dir dir = match dir with
                | (-1,0) -> (0,1)
                | (0,1) -> (1,0)
                | (1,0) -> (0,-1)
                | (0,-1) -> (-1,0)
                | _ -> printf "direction doesn't exist"; (0,0)
        in
        let get_next_pos start dir =
                let sy, sx = start in
                let dy, dx = dir in
                let y, x = (sy+dy),(sx+dx) in

                if 0 <= y && y < (max_y) && 0 <= x && x < max_x then
                        try let _ = grid.(y).(x) in Some (y,x) with
                        | Invalid_argument _ -> None
                else
                        None
        in

        let prev_pos_dir_tb = (Hashtbl.create 1000) in
        let rec walk_in start dir =
                let sy, sx = start in

                match get_next_pos start dir with
                | Some (y,x) when grid.(y).(x) = '.' -> grid.(y).(x) <- 'X'; walk_in (y,x) dir
                | Some (y,x) when grid.(y).(x) = '#' || grid.(y).(x) = 'O' -> Some start
                | Some (y,x) when grid.(y).(x) = 'X' ->

                        (* WARN: use curr pos to check if visited not next pos*)
                        (match Hashtbl.find_opt prev_pos_dir_tb ((sy,sx),dir) with
                        | Some _ -> Some (-888,-888)
                        | None -> walk_in (y,x) dir)
                | Some _ -> printf "error when walking in line"; Some (420,420)
                | None -> None
        in

        let rec walk start dir = 
                match walk_in start dir with
                | Some (-888,-888) -> 1
                | Some pos -> (Hashtbl.add prev_pos_dir_tb (start,dir) true); walk pos (next_dir dir)
                | None -> 0
        in
        walk start dir 

let () =
        printf "starting readin\n";
        let grid = make_grid txt_big in
        let dup g = Array.map (fun l -> Array.copy l) g in
        let count = ref 0 in
        printf "starting looping\n";
        Array.iteri (fun i l ->
                l
                |> Array.iteri (fun j c ->
                        if c = '.' then
                                let tmp = dup grid in
                                tmp.(i).(j) <- 'O';
                                count := !count + (detect_loop tmp);
                                (* printf "%d:%d\n" i j; *)
                );
        ) grid;
        printf "loops found:%d\n" !count;;
