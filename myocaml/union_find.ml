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
  (findSet x l) = (findSet y l)

let print_union {treeArr} =
  Array.iter (fun x -> print_int x; print_string " ") treeArr

let empty_union = create 0

let union_list l =
  match l with
  | [] -> []
  | x :: tl as l ->
    let a = Array.of_list l in
    (*let n = Array.length a - 1 in*)
    (*for i = 0 to 0 do*)
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
    (*done;*)
    let l = Array.to_list a in
    l

(*let union_list_print l list_given =
  match l with
    | [] -> []
    | h :: tl -> List.fold_left (fun list t ->
      (*print_string "\nlist: ";
      print_list list; print_string " ";*)
      print_string "\nh: "; print_int h;print_string " ";
      print_string "\nt: "; print_int t;print_string " ";
      let u = union h t list in
      print_string "\nresult u: ";
      print_list u; 
      print_string "\nlist: ";
      print_list list; print_string "\n";
      u
    ) list_given tl*)

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

(**********************************************************************************)
(*TEST*)

open Output

let l = [0;1;2;3;4]

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
