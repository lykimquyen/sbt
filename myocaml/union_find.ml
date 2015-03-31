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

let dump a = Array.iteri (Printf.fprintf stdout "%i:%i;") a 

let findSet e a =
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
    
let union x y a =
  let root_x = findSet x a in
  let root_y = findSet y a in
  (*let _ = Printf.fprintf stdout "UNION %i %i %i %i\n" x y root_x root_y in *)
  let _ = a.(root_y) <- root_x in 
  (*let _ = dump a in *)
  a 

module IntMap = Map.Make (struct type t = int let compare = compare end)

let eq_classes_union a =
  let size = Array.length a in 
  let classes = IntMap.empty in 
  let rec aux k (classes,union_list) = 
     if k < 0 
     then 
       (classes,union_list)
     else 
       let rep = findSet k a in 
       let old = 
         try 
           IntMap.find rep classes 
         with 
             Not_found -> []
       in 
       let classes = IntMap.add rep (k::old) classes in 
       aux (k-1) (classes,union_list)
  in 
  let classes,a = aux (size-1) (classes,a) in 
  let classes =
    IntMap.fold 
      (fun k list output -> 
        match list with 
          | [] -> output
          | _ -> 
            let _ = Printf.fprintf stdout "%i:{" k in 
            let _ = List.iter (Printf.fprintf stdout "%i,") list in 
            let _ = Printf.fprintf stdout "\n" in 
            list::output)
      classes []
  in 
  classes,a

let component l a =
   match l with
   | [] -> a
   | x :: tl  ->
     List.fold_left 
       (fun a y ->
       let a = union x y a in
       let _ = eq_classes_union a in
       print_string "\n";
       a)
       a tl

(**********************************************************************************)
(*TEST*)

open Output
let print_union {treeArr} =
  Array.iter (fun x -> print_int x; print_string " ") treeArr

let l = [0;1;2;3]
let a = 
  let size = List.length l in
  Array.init size (fun i -> i)

let print_component =
  print_string "1) component: \n";
  let _ = component l a in
  ()

