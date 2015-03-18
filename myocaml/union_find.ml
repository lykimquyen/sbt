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

let findSet e union_find =
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
let union x y union_find =
  let root_x = findSet x union_find in
  let root_y = findSet y union_find in
  let treeArr = union_find.treeArr in
  treeArr.(root_x) <- root_y;
  union_find

let is_equivalence x y union_find =
  (findSet x union_find) = (findSet y union_find)

let print_union {treeArr} =
  Array.iter (fun x -> print_int x; print_string " ") treeArr

let empty_union = create 0

let test l union_find =
  match l with
    | [] -> empty_union
    | t:: tl ->
      List.fold_left (fun union_c t' -> union t t' union_c) union_find tl

let create_list_union l =
  List.fold_left (fun u t -> create t) l

(**********************************************************************************)
(*TEST*)

open Output

let union_find = create 2

let print_union1 =
  print_string "1) Union_find: ";
  print_union union_find; print_string "\n";
  let u = union 0 1 union_find in
  print_string "Result: ";
  print_union u;
  print_string "\n"

let print_findSet =
  print_string "2) Union_find: ";
  print_union union_find; print_string "\n";
  print_string "Root of x in union_find is 'findSet': ";
  let s = findSet 0 union_find in
  print_int s; print_string "\n"

let union_find2 = create 2 (*old_list*)
let l = [0;1] (*sites_list*)

let print_test =
  print_string "3) Union_find: ";
  print_union union_find2; print_string "\n";
  print_string "list:"; print_list l; print_string "\n";
  print_string "Test: ";
  let u = test l union_find2 in
  print_union u; print_string "\n" 
