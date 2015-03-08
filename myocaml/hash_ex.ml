(****************************************************************************************)
(* 08-03-2015                                                                           *)
(* Example about hashtable                                                              *)
(****************************************************************************************)

(* remove duplicate *)

(* straightforward
This function will print the result in the reverse order. Using List.rev to reorder the list
 *)

let hash_uniquesOnly l =
  let seen = Hashtbl.create 17 (* create an initial hastbl *)
  and uniq = ref [] in (* create the place to store the result *)
  List.iter (fun x ->
    if not (Hashtbl.mem seen x) (* if x is not a member in seen *)
    then (Hashtbl.add seen x 1; (* then add x into seen at position 1 *)
	  uniq := (x :: !uniq)) (* return the value by adding x as a head of this list*)
  ) (List.rev l); (* walk through this list*)
  !uniq (* print the result out *)

let fold_uniquesOnly l =
  let seen = Hashtbl.create 17 in
  List.iter (fun x ->
    Hashtbl.replace seen x 1)
    (List.rev l);
  Hashtbl.fold (fun k v b -> k :: b) seen []

(**********************************************************************************)
(*TEST*)
open Output

let l1 = [1;2;3;3;2;2;1;2;3;4]

let print_hash_uniquesOnly =
  print_string "9) List original: ";
  print_list l1; print_string "\n";
  let l = hash_uniquesOnly l1 in
  print_string "List of hash_uniquesOnly is:";
  print_list l;
  print_string "\n"

let print_fold_uniquesOnly =
  print_string "10) List original: ";
  print_list l1; print_string "\n";
  let l = fold_uniquesOnly l1 in
  print_string "List of fold_uniquesOnly is:";
  print_list l;
  print_string "\n"
