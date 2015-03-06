(****************************************************************************************)
(*Union find*)
(*
type union_find = {id: int arry;
                   size: int arry}

let create_union n = 
  { id = Array.init n (fun i -> i);
    size = Array.init n (fun i -> 1)
  }

let union {id; _} p q  =
  let (value_p, value_q) = (id.(p), id.(q)) in
  let rec union_find id i =
    if i < Array.length id
    then
      begin
        if i != q && id.(i) = value_p
        then
          id.(i) <- value_q;
        union_find id (i+1)
      end
    else
      print_string "end of union \n"
  in union_find 0

let is_connected u p q = u.(p) = u.(q)*)

(*union find with rank*)
(*
type union_find =
    {id: int array;
     size: int array}

let create_union n =
  {
    id = Array.init n (fun i -> i);
    size = Array.make n 1
  }

let find_parent parent i =
  let rec find j =
    if parent.(j) = j
    then j
    else find parent.(j)
  in find i

let union_rank {id; size} i j =
  let parent_i = find_parent id i in
  let parent_j = find_parent id j in
  if size.(parent_i) < size.(parent_j)
  then
    begin
      id.(parent_i) <- id.(parent_j);
      size.(parent_j) <- size.(parent_j) + size.(parent_i)
    end
  else
    begin
      id.(parent_j) <- id.(parent_i);
      size.(parent_i) <- size.(parent_i) + size.(parent_j)
    end

let is_connected_ranked {id; _} i j =
  (find_parent id i) = (find_parent id j)*)

type union_find =
    {id: int array;
     size: int}

let create_union n =
  {
    id = Array.init n (fun i -> i);
    size = n (*FIXME*)
  }

let find_parent parent i =
  let rec find j =
    if parent.(j) = j
    then j
    else find parent.(j)
  in find i

let union_rank {id; size} i j =
  let parent_i = find_parent id i in
  let parent_j = find_parent id j in
  if parent_i < parent_j
  then
    begin
      id.(parent_i) <- id.(parent_j);
      parent_j = parent_j + parent_i
    end
  else
    begin
      id.(parent_j) <- id.(parent_i);
      parent_i = parent_i + parent_j
    end

let is_connected_ranked {id; _} i j =
  (find_parent id i) = (find_parent id j)

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

let length_list_list lists =
  List.map (fun list -> list, List.length list) lists
    
(*sorting the length of a list of list, increasing order *)

let length_sort lists =
  (*let lists = List.map (fun list -> list, List.length list) lists in*)
  let lists = List.sort_uniq (fun a b -> compare (snd a) (snd b))
                        (length_list_list lists) in
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

(*Construct a set from a list *)

module Int_set = Set.Make (struct
                              type t = int
                              let compare = compare
                            end)

(*iters through a list to construct a set*)
let set_of_list = List.fold_left (fun acc x ->
                                  Int_set.add x acc) Int_set.empty
                                 
(**********************************************************************************)
(*TEST*)

let l = [1;2;3;3;2;2;1;2;3;4]
let ls = [[0;1;2;2;1;2;1;0];[3];[3;4;5;6;6]]
let ls_no_sort = [[4;5;3;5;7];[2;1];[5;6;7];[0];[4;5;6;6;6;5]]
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

let list_rev_fold = List.rev (List.fold_right (fun x acc -> [x]@acc) l [])

let list_fold_left = List.fold_left (fun x acc -> x + acc) 0 l
  
let print_list = List.iter (fun i -> Printf.printf "%i " i)

let rec print_list_list ls =
  match ls with
  | [] -> ()
  | is :: ls' ->
     let _ = print_string "["; List.iter (fun i -> Printf.printf " %i " i) is in
     print_string "]";
     print_list_list ls'     
		         
let print_test = print_list_list ls
                                     
let rec print_pair pair =
  match pair with
  | [] -> ()
  | (p, i) :: ls ->
     let _ = print_string "\nList:[ "; print_list p; print_string "]\nLength: ";
	     print_int i; print_string "\n"
     in
     print_pair ls
	        
let print_length_pair_test =
  let l = length_list_list ls in
  print_pair l
             
let print_sort_test =
  print_string "1) List not sort: [";
  print_list_list ls_no_sort; print_string "]\n";
  let s = length_sort ls_no_sort in
  print_string "a) List sorted descreasing order: [" ;
  print_list_list s; print_string "]\n"
                                      
let print_sort_dup_test =
  let s = length_sort_dups ls_no_sort in
  print_string "b) List sorted descreasing order and duplicates: [" ;
  print_list_list s; print_string "]\n"
                                      
let print_remove_dups = 
  print_string "3) List with duplicate elements: ["
  ; print_list l; print_string "]\n"
  ; print_string "List after remove duplicate elements: ["
  ; print_list (remove_dups l)
  ; print_string "]\n"
                 
let print_remove_dups_lists =
  print_string "4) List of list with duplicate elements: ["
  ; print_list_list ls; print_string "]\n"
  ; print_string "List after remove duplicate elements in a list of list: ["
  ; print_list_list (remove_dups_lists ls)
  ; print_string "]\n"
                 
let print_remove_dups_list =
  print_string "5) List of list with duplicate elements: ["
  ; print_list_list ls; print_string "]\n"
  ; print_string "List after remove duplicate elements in a list of list: [";
  let l = remove_dups_lists ls in
  print_list_list l
  ; print_string "]\n"
                 
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

(* Remove a subset in a list of lists *)
let map_filter l1 ls = List.map (List.filter (fun x -> List.mem x l1))ls

let result_filter ls =
  match ls with
  | [] -> []
  | l1 :: ls' -> map_filter l1 ls'
  
let result ls =
	let ls' = result_filter ls in
	List.filter (fun l -> not (List.mem l ls')) ls

let l10 = [[1;2;3;4];[1;2];[1;2;3];[0]]

let print_result_filter =
  let r = result_filter l10 in
  print_string "11) List of lists test:"; print_list_list l10; print_string "\n";
  print_string "a) Return the subset:";
  print_list_list r; print_string"\n"

let print_result =
  let r = result l10 in
  print_string "b) Remove the subset:";
  print_list_list r; print_string "\n"

(*Test list flatten*)
let print_flatten =
  print_string "12) List of lists before flatten: ";
  print_list_list l10; print_string "\n";
  let l = List.flatten l10 in
  print_string "Print list flatten: ";
  print_list l;
  print_string "\n"

(*remove duplicate in a list of lists by using a hashtable*)
let age = Hashtbl.create 3;;

Hashtbl.replace age "Nat" 24;;

let assoc_list2hashtbl assoc_list =
  let h = Hashtbl.create 0 in
  List.iter (fun (k,v) -> Hashtbl.replace h k v) assoc_list;
  h

let food_color = assoc_list2hashtbl
                   ["Apple", "red"; "Banana", "yellow"]
                         
(*let print_hashtbl =
  Hashtbl.iter (fun f _ -> print_endline f) food_color*)
