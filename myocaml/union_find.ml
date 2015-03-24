(****************************************************************************************)
(* 08-03-2015                                                                           *)
(* Union find                                                                           *)
(****************************************************************************************)

(* algorithm of union find (dijoint-set data structure)
- find: which subset a particular element is in.
- union: join two subsets into a single subset.
Find(x) returns the representative (a fixed element of each set) of the set that 'x' belongs to.
Union takes two set representatives as its arguments.
*)
      
(**********************************************************************************)
(* union find with path compression without rank *)

type union_find =
    {
      treeArr: int array
    }

let create n =
  {
    treeArr = Array.init n (fun i -> i)
  }

(* findSet(x): which return a pointer to the representative of the set
   containing x. Since the set are disjoint, x containted in one set
   only. Therefore, the returned representative can be uniquely determined.
   e: is the element we need to find. 
   returns the root of the tree that containing x
*)

let findSet e list =
  let a = Array.of_list list in
  let union_find = {treeArr = a} in
  let treeArr = union_find.treeArr in
  let pointToRoot root =
    List.iter (fun i -> treeArr.(i) <- root) in
  let rec helper e l =
    let parent = treeArr.(e) in
    if e <> parent
    then
      helper parent (e::l)
    else
      begin
        (* base case: we hit the root node make all collected nodes on the
           path point to the root.  and return the root afterwards *)
        pointToRoot parent l;
        parent;
      end
  in
  helper e []
    
(* int -> int -> union_find -> union_find *)
let union x y l =
  let root_x = findSet x l in
  let root_y = findSet y l in
  let a = Array.of_list l in
  let n = Array.length a - 1 in
  for i = 0 to n do
    let union_find = {treeArr = a} in
    let treeArr = union_find.treeArr in
    treeArr.(root_y) <- root_x
  done;
  let l = Array.to_list a in
  l

let is_equivalence x y l =
  (findSet x l) == (findSet y l)

let print_union {treeArr} =
  Array.iter (fun x -> print_int x; print_string " ") treeArr

let empty_union = create 0
  
let union_list l =
  match l with
  | [] -> []
  | x :: tl as l ->
    let a = Array.of_list l in
    List.iter (fun y ->
      let root_x = findSet x l in
      let root_y = findSet y l in
      let union_find = {treeArr = a} in
      let treeArr = union_find.treeArr in
      if is_equivalence x y l
      then ()
      else
	treeArr.(root_x) <- root_y;
    ) tl;
    let l = Array.to_list a in
    l

let eq_class m i =
  let column = m.(i) in
  let set = ref [] in
  Array.iteri (
    fun j _ ->
      if i = j || column.(j)
      then
	set := j :: !set)
    column;
  !set

let eq_classes m =
  let classes = ref [] in
  Array.iteri begin fun e _ ->
    if not (List.exists (List.mem e) !classes) then
      classes := eq_class m e :: !classes
  end m;
  !classes;;

let position x =
  let rec aux k = function
    | [] -> raise Not_found
    | y :: ys ->
      if x = y
      then k
      else aux (k + 1) ys
  in aux 0

let option_value l s =
  try let p = position s l
      in Some p
  with Not_found -> None
    
(*let compute_matrix (l: int list list) =
  let len = List.length l in
  let boolmat = Array.make_matrix len len false in
  List.iteri (fun s strs ->
    match option_value l s with
      | Some p1 -> List.iter (fun t ->
	match option_value l t with
	  | Some p2 -> boolmat.(p1).(p2) <- true
	  | None -> ()) strs
      | None -> ()
  ) l;
  boolmat*)
  
let punion_list l =
  match l with
  | [] -> []
  | x :: tl as l ->
    let a = Array.of_list l in
    List.iter (fun y ->
      let root_x = findSet x l in
      print_string "\nroot_x: ";
      print_int root_x;
      let root_y = findSet y l in
      print_string " root_y: ";
      print_int root_y; print_string "\n";
      let union_find = {treeArr = a} in
      let treeArr = union_find.treeArr in
      treeArr.(root_x) <- root_y;
      print_string "union_find: ";
      print_union union_find; print_string "\n"
    ) tl;
    let l = Array.to_list a in
    l

let rec union_lists ls =
  match ls with
    | [] -> []
    | l :: ls' ->
      let _ = union_list l in
      union_lists ls'
	
(*let list_of_union {treeArr} =
  let rec list_of_union i res =
    if i < 0
    then res
    else
      list_of_union (i - 1) (Array.unsafe_get treeArr i :: res)
  in
  list_of_union (Array.length treeArr - 1) []

let union_of_list l = 
  {treeArr = Array.of_list l}
*)

let rec find p x =
  if p.(x) = x
  then x
  else
    let y = find p (p.(x)) in
    p.(x) <- y;
    y;;

let union2 x y p =
  p.(find p y) <- p.(find p x)

let eq_classes1 eq_class =
  let classes = ref [] in
  classes := eq_class :: !classes
  
(*let eq_classes m =
  let classes = ref [] in
  Array.iteri begin fun e _ ->
    if not (List.exists (List.mem e) !classes) then
      classes := eq_class m e :: !classes
  end m;
  !classes;; *)
(**********************************************************************************)
(*TEST*)

open Output

let l = [0;1;2;3;4]
let ls1 = [[0;1];[2;3]]
  
let m1 = [|[|false; true; true|];
	  [|false; false ; true|];
	  [|true; false; false|]
	|]

let print_column =
  let column = m1.(0) in
  print_string "eq_class: ";
  print_bool_array column;
  print_string "\n"

let print_eq_classes =
  let e = eq_classes m1 in
  print_string "eq_classes: ";
  print_list_list e; print_string "\n"

let print_union_find =
  print_string "1) List: ";
  print_list l; print_string "\n";
  let l = union 0 1 l in
  print_string "Result union (0, 1): ";
  print_list l;
  print_string "\n"
    
let print_findSet =
  print_string "2) List: ";
  print_list l; print_string "\n";
  print_string "Root of x in union_find is 'findSet': ";
  let s = findSet 1 l in
  print_int s; print_string "\n"
    
let l1 = [0;1;2;3]

let print_union_list =
  print_string "3) List: ";
  print_list l1; print_string "\n";
  print_string "Result: ";
  let l = union_list l1 in
  print_list l; print_string "\n"

let print_union_lists =
  print_string "4) Lists: ";
  print_list_list ls1; print_string "\n";
  print_string "Result: ";
  let l = union_lists ls1 in
  print_list_list l; print_string "\n"
  
(*
let union_find = create 5

let print_union1 =
  print_string "1) Union_find: ";
  print_union union_find; print_string "\n";
  let u = union 0 1 union_find in
  print_string "Result union (0, 1): ";
  print_union u;
  print_string "\n"

let print_findSet =
  print_string "2) Union_find: ";
  print_union union_find; print_string "\n";
  print_string "Root of x in union_find is 'findSet': ";
  let s = findSet 0 union_find in
  print_int s; print_string "\n"

let union_find3 = create 6 (*old_list*)
let l3 = [2;3] (*sites_list*)

let print_union_list =
  print_string "4) Union_find: ";
  print_union union_find3; print_string "\n";
  print_string "list:"; print_list l3; print_string "\n";
  print_string "Result: ";
  let u = union_list l3 union_find3 in
  print_union u; print_string "\n"

let union_find4 = create 6 (*old_list*)
let l4 = [2;5] (*sites_list*)

let print_union_list_print =
  print_string "5) Union_list_print: ";
  print_union union_find4; print_string "\n";
  print_string "list:"; print_list l4; print_string "\n";
  print_string "Result of union_list_print: ";
  union_list_print l4 union_find4*)
