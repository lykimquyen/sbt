(****************************************************************************************)
(* 08-03-2015                                                                           *)
(* Set                                                                                  *)
(****************************************************************************************)
open Output

(*Construct a set from a list *)

module Int_set = Set.Make (struct
                              type t = int
                              let compare = compare
                            end)

(*iters through a list to construct a set*)
let set_of_list = List.fold_left (fun acc x ->
  Int_set.add x acc) Int_set.empty

(*do subset in a list of lists*)
let rec subset_lists ls =
  match ls with
  | [] -> print_string "nothing\n"
  | l1 :: ls' ->
     List.iter
       (fun l2 ->
        let s1 = set_of_list l1 in
        let s2 = set_of_list l2 in
        print_string "List l1: [";
        print_list l1;
        print_string "]\nList l2: [";
        print_list l2;
        print_string "]\n";

        if Int_set.subset s1 s2
        then
          print_string "Subset in a lists: [";
        Int_set.iter (fun elt -> Printf.printf " %i " elt) s2;
        print_string "]\n"
       ) ls'

let rec subset_lists2 ls =
  match ls with
  | [] -> print_string "empty\n"  
  | _ :: [] -> print_string "one element in a list\n"
  | l1 :: (l2 :: ls') ->
     let s1 = set_of_list l1 in
     let s2 = set_of_list l2 in
     print_string "List l1: [";
     print_list l1;
     print_string "]\nList l2: [";
     print_list l2;
     print_string "]\n";
     if Int_set.subset s1 s2
     then
     print_string "Subset in a lists: [";
     Int_set.iter (fun elt -> Printf.printf " %i " elt) s2;
     print_string "]\n";
     subset_lists2 ls'

(* convert a list into a set *)
module StringSet = Set.Make (String)

(*elt list -> t *)
let set_to_list l =
  List.fold_left (fun set elem ->
                  StringSet.add elem set
    ) StringSet.empty l


(**********************************************************************************)
(*TEST*)
(*l1, l2 for testing the intersection of two sets*)
let l1 = [3;4;5;6;7]
let l2 = [1;3;5;7;9]
(*l3, l4 for testing the union of two sets*)
let l3 = [1;2;3;4]
let l4 = [1;2]
(*l5, l6 for testing the subset of two sets*)
let l5 = [1;2]
let l6 = [1;2;3]
(*l7 for testing the subset_lists*)
let l7 = [[1;2];[1;2;3];[1;2;4]]
let l8 = [[1;2];[1;2;4;5];[1;2;4]]
(*l9 for remove test*)
let l9 = [[1;2;3;4];[1;2];[1;2;3;4];[0]]

(*convert the list into a set, and test the intersection of two sets*)
let print_inter_set =
  let s1 = set_of_list l1 in
  let s2 = set_of_list l2 in
  let s3 = Int_set.inter s1 s2 in
  print_string "6) The lists are:\n";
  print_string "l1: [";
  print_list l1; print_string "]\n";
  print_string "l2: [";
  print_list l2; print_string "]\n";
  
  print_string "Intersection of two sets: ";
  Int_set.iter (fun elt -> Printf.printf " %i " elt) s3;
  print_string "\n"

(*convert the list into a set, and test the union of two sets*)
let print_union_set =
  let s1 = set_of_list l3 in
  let s2 = set_of_list l4 in
  let s3 = Int_set.union s1 s2 in
  print_string "7) The lists are:\n";
  print_string "l3: [";
  print_list l3; print_string "]\n";
  print_string "l4: [";
  print_list l4; print_string "]\n";

  print_string "Union of two sets: ";
  Int_set.iter (fun elt -> Printf.printf " %i " elt) s3;
  print_string "\n"

(*convert the list into a set, and test the union of two sets*)
let print_subset =
  let s1 = set_of_list l5 in
  let s2 = set_of_list l6 in
  let s3 = Int_set.subset s1 s2 in
  print_string "8) The lists are:\n";
  print_string "l5: [";
  print_list l5; print_string "]\n";
  print_string "l6: [";
  print_list l6; print_string "]\n";
  if s3
  then
  print_string "Print the superset and remove the subset of two sets:[";
  Int_set.iter (fun elt -> Printf.printf " %i " elt) s2;
  print_string "]\n"

(* Keep this function for learning *)
let print_subset_lists =
  print_string "9) Subset in a lists: \n";
  subset_lists l7;
  print_string "\n"

(* Keep this function for learning; it wrong because it missing the rest of the list *)
let print_subset_lists2 =
  print_string "10) Wrong subset in a lists: \n";
  subset_lists2 l7;
  print_string "\n"
