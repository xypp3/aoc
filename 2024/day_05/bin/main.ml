open Printf

let file_to_string filename =
        In_channel.(with_open_text filename input_all)

let txt_big = file_to_string "input_big.txt";;
txt_big
let txt_small_1 = {|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|};;
txt_small_1

let check_update update rules =
        let follows_rules num prev_bans = 
                List.find_map (fun (banned,a) -> if banned = num then Some (banned,a) else None) prev_bans
        in
        let make_prev_bans num rules =
                List.filter (fun (_,b) -> num = b) rules
        in

        let rec check u prev_bans = match u with  
                | [] -> true
                | x :: xs -> match follows_rules x prev_bans with
                        | Some _ -> false
                        | None -> check xs ((make_prev_bans x rules) @ prev_bans)
        in
        check update [];;

let find_middle update =
        (List.nth update ((List.length update)/2))

let count_correct updates rules =
        List.fold_left (fun acc u -> match check_update u rules with
                | false -> acc
                | true -> acc + (find_middle u)
        ) 0 updates

let make_rules lines =
        let split ls = (int_of_string (List.nth ls 0), int_of_string (List.nth ls 1)) in
        List.map (fun a -> split (String.split_on_char '|' a)) lines

let make_updates lines =
        List.map (fun l -> List.map (fun n -> int_of_string n) (String.split_on_char ',' l) ) lines

let make_types str =
        let lines = String.split_on_char '\n' str in
        let split_ind = match List.find_index (fun c -> c = "") lines with
                | Some i -> i
                | None -> raise Not_found
        in
        let rules = make_rules (List.filteri (fun i _ -> i < split_ind) lines) in
        let updates = make_updates (List.filteri (fun i s -> i > split_ind && s <> "") lines) in
        (updates,rules)

let () =
        let updates, rules = make_types txt_small_1 in
        List.iter (fun (pre,post) -> printf "%d->%d\n" pre post) rules;
        List.iter (fun line -> List.iter (fun d -> printf "%d;" d) line; printf "\n") updates;
        printf "update 1:%B\n" (check_update (List.nth updates 0) rules);
        printf "findmiddle 1:%d\n" (find_middle (List.nth updates 0));
        printf "part1-small:%d\n" (count_correct updates rules);
        let updates, rules = make_types txt_big in
        printf "part1-big:%d\n" (count_correct updates rules)


let fix_update update rules =
        let points_to rules a b =
                match List.find_opt (fun (pre,post) -> pre = a && post = b) rules with
                | Some _ -> 1
                | None -> 0
        in 
        List.sort (points_to rules) update

let part_2 updates rules =
        List.fold_left (fun acc u -> match check_update u rules with
                | false -> acc + (find_middle (fix_update u rules))
                | true -> acc
        ) 0 updates


let () =
        let updates, rules = make_types txt_small_1 in
        printf "part2-small:%d\n" (part_2 updates rules);
        let updates, rules = make_types txt_big in
        printf "part2-big:%d\n" (part_2 updates rules)

