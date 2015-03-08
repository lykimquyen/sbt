(****************************************************************************************)
(* 08-03-2015                                                                           *)
(* Examples about list                                                                  *)
(****************************************************************************************)

(****************************************************************************************)
(*compute the length of the int list list*)

let length_list list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux (count + 1) acc t
      else
        aux 0 ((a, count + 1) :: acc) t
  in aux 0 [] list

(* List map will output as a reverse order, so to print the normal
   order in the orginal list use the List.rev_map *)

let length_list_list lists =
  List.map (fun list -> list, List.length list) lists
    
(*sorting the length of a list of list, increasing order *)

let length_sort lists =
  let lists = List.rev_map (fun list -> list, List.length list) lists in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) lists in
  List.rev_map fst lists (*Keep the list in a pair (list,_) *)

(*Remove duplicate inside a list*)
let rec remove_dups l =
  match l with
  | [] -> [] 
  | h :: t ->
    h :: (remove_dups (List.filter (fun x -> x <> h)t))
	    
let rec remove_dups_lists ls =
  match ls with
  | [] -> []
  | h :: t ->
     let h' = remove_dups h in
     h' :: (remove_dups_lists (List.filter(fun x -> 
       let x' = remove_dups x in x' <> h') t))
             
(*sorting and remove duplicate*)
      
let length_sort_dups lists =
  let remove_lists = remove_dups_lists lists in
  let lists = List.rev_map (fun list -> list, List.length list) remove_lists in
  let lists = List.sort (fun a b -> compare (snd a) (snd b)) lists in
  List.rev_map fst lists

let list_rev_fold l = List.rev (List.fold_right (fun x acc -> [x]@acc) l [])

let list_fold_left l = List.fold_left (fun x acc -> x + acc) 0 l

(* Remove a subset in a list of lists, this is very expensive, because it will go through
 the list many times *)

(* return subsets that are a subset in a set *)
let map_filter_mem l1 ls = List.map (List.filter (fun x -> List.mem x l1))ls

let result_filter_mem ls =
  match ls with
  | [] -> []
  | l1 :: ls' -> map_filter_mem l1 ls'

(* return subsets that are not subsets in a set*)
let result_mem ls =
  let ls' = result_filter_mem ls in
  List.filter (fun l -> not (List.mem l ls')) ls

(* not *)
(* return subset that are not the member of sets *)
let map_filter_not_mem l1 ls = List.map (List.filter (fun x -> not (List.mem x l1)))ls

let result_filter_not_mem ls =
  match ls with
  | [] -> []
  | l1 :: ls' -> map_filter_not_mem l1 ls'

(* return all subsets except the subset that are not a member *)
let result_not_mem ls =
  let ls' = result_filter_not_mem ls in
  List.filter (fun l -> not (List.mem l ls')) ls

(* another implementation about duplication The most "natural" way to
   do this is by walking the list and looking for duplicates of each
   item *)
    
let rec uniquesOnly l =
  (* this is a member function *)
  let rec contains x l =
    match l with
      | [] -> false
      | h :: t ->
	if x = h
	then true
	else contains x t
  in
  match l with
    | [] -> []
    | h :: t ->
      if contains h t
      then uniquesOnly t
      else h :: (uniquesOnly t)

let rec uniquesOnly_filter l =
  match l with
    | [] -> []
    | h :: t -> h :: (uniquesOnly_filter (List.filter ((<>) h)t))
    
(**********************************************************************************)
(*TEST*)

open Output

let l1 = [1;2;3;3;2;2;1;2;3;4]
let ls1 = [[0;1;2;2;1;2;1;0];[3];[3;4;5;6;6]]
let ls2 = [[1;2;3;4];[1;2];[1;2;3];[0]]
let ls_no_sort = [[4;5;3;5;7];[2;1];[5;6;7];[0];[4;5;6;6;6;5]]
	        
let print_length_pair_test =  
  let l = length_list_list ls1 in
  print_string "1) List original ls: ";
  Output.print_list_list ls1;
  print_string "\nList result: ";
  Output.print_pair l; print_string "\n"

(* sort *)
let print_sort_test =
  print_string "2) List not sort: [";
  print_list_list ls_no_sort; print_string "]\n";
  let s = length_sort ls_no_sort in
  print_string "a) List sorted descreasing order: [" ;
  print_list_list s; print_string "]\n"

(* duplicates *)                              
let print_sort_dup_test =
  let s = length_sort_dups ls_no_sort in
  print_string "b) List sorted descreasing order and remove duplicates: [" ;
  print_list_list s; print_string "]\n"
                                      
let print_remove_dups = 
  print_string "3) List with duplicate elements: ["
  ; print_list l1; print_string "]\n"
  ; print_string "List after remove duplicate elements: ["
  ; print_list (remove_dups l1)
  ; print_string "]\n"
                 
let print_remove_dups_lists =
  print_string "4) List of list with duplicate elements: ["
  ; print_list_list ls1; print_string "]\n"
  ; print_string "List after remove duplicate elements in a list of list: ["
  ; print_list_list (remove_dups_lists ls1)
  ; print_string "]\n"

(* subset *)
let print_result_filter_mem =
  let r = result_filter_mem ls2 in
  print_string "6) List of lists test (result_filter_mem):";
  print_list_list ls2; print_string "\n";
  print_string "a) Return the subset:";
  print_list_list r; print_string"\n"

let print_result_mem =
  let r = result_mem ls2 in
  print_string "b) Remove the subset (result_mem):";
  print_list_list r; print_string "\n"

let print_result_filter_not_mem =
  let r = result_filter_not_mem ls2 in
  print_string "7) List of lists test (result_filter_not_mem):";
  print_list_list ls2; print_string "\n";
  print_string "a) Return the not a subset:";
  print_list_list r; print_string"\n"

let print_result_not_mem =
  let r = result_not_mem ls2 in
  print_string "b) Return (result_not_mem):";
  print_list_list r; print_string "\n"

(* list flatten function *)
let print_flatten =
  print_string "8) List of lists before flatten: ";
  print_list_list ls2; print_string "\n";
  let l = List.flatten ls2 in
  print_string "Print list flatten: ";
  print_list l;
  print_string "\n"

(* test the uniquesOnly *)
let print_uniquesOnly =
  print_string "9) List original: ";
  print_list l1; print_string "\n";
  let l = uniquesOnly l1 in
  print_string "List of uniquesOnly is:";
  print_list l;
  print_string "\n"

let print_uniquesOnly_filter =
  print_string "9) List original: ";
  print_list l1; print_string "\n";
  let l = uniquesOnly_filter l1 in
  print_string "List of uniquesOnly_filter is:";
  print_list l;
  print_string "\n"
