open Printf

let file_to_string filename =
        In_channel.(with_open_text filename input_all)

let txt_big = file_to_string "big.txt";;
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
        let get_postcond num = List.filter (fun (_,post) -> post = num ) rules in
        let rec check u prev =
        match u with
        | [] -> true
        | x :: _ ->  let (_,post) = List.nth (get_postcond x) 0 in
                match List.find_opt (fun a -> post = a) prev with 
                | None -> false
                | Some _ -> check (List.tl u) (x :: prev) in
        check update []

let find_middle update =
        int_of_string (List.nth update ((List.length update)/2))

let count_correct updates rules =
        List.fold_left (fun acc u -> match check_update u rules with
                | false -> acc
                | true -> acc + (find_middle u)
        ) updates

let make_rules str_ls =
        let splitted = List.map (fun a -> let sides = String.split_on_char '|' a) in

let make_types str =
        let lines = String.split_on_char '\n' str in
        let two_parts = String.split_on_char '\n' str in
