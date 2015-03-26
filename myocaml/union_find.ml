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
    
let union_l x y l =
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

(*union-find with array type *)
let findSet_array e {treeArr}=
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
    
let union_array x y union_find =
  let root_x = findSet_array x union_find in
  let root_y = findSet_array y union_find in
  let treeArr = union_find.treeArr in
  treeArr.(root_y) <- root_x;
  treeArr

let eq_classes' l =
  let a = Array.of_list l in
 (* Let's first create an array for storing the classes *)
 let classes = Array.make (Array.length a) [] in
 (* Let's now populate it!
    I'm going backwards in the array to have nicer printing *)
 for i = (Array.length classes) - 1 downto 0
 do classes.(a.(i)) <- i :: (classes.(a.(i))) done;
 (* And now the printing *)
 Array.iter (function
   | [] -> ()
   | h::t -> Printf.printf "{%d%a}" h
             (fun c -> List.iter (fun x -> Printf.fprintf c ",%i" x)) t
   ) classes

let eq_classes_union l =
  (*let union = union_list l in*)
  let a = Array.of_list l in
 (* Let's first create an array for storing the classes *)
 let classes = Array.make (Array.length a) [] in
 (* Let's now populate it!
    I'm going backwards in the array to have nicer printing *)
 for i = (Array.length classes) - 1 downto 0
 do classes.(a.(i)) <- i :: (classes.(a.(i))) done;
 (* And now the printing *)
 Array.iter (function
   | [] -> ()
   | h::t -> Printf.printf "{%d%a}" h
             (fun c -> List.iter (fun x -> Printf.fprintf c ",%i" x)) t
   ) classes

let eq_classes a =
 (* Let's first create an array for storing the classes *)
 let classes = Array.make (Array.length a) [] in
 (* Let's now populate it!
    I'm going backwards in the array to have nicer printing *)
 for i = (Array.length classes) - 1 downto 0
 do classes.(a.(i)) <- i :: (classes.(a.(i))) done;
 (* And now the printing *)
 Array.iter (function
   | [] -> ()
   | h::t -> Printf.printf "{%d%a}" h
             (fun c -> List.iter (fun x -> Printf.fprintf c ",%i" x)) t
   ) classes

let component l =
   let result = ref [] in
   match l with
   | [] -> []
   | x :: tl  ->
      List.iter (fun y ->
                 result := union_l x y l :: !result) tl;
      !result

let component' l =
   let result = ref [] in
   match l with
   | [] -> []
   | x :: tl  ->
     List.iter (fun y ->
       let u = union_l x y l in
       let _ = eq_classes_union u in
       print_string "\n";
       result := u :: !result       
     ) tl;
     !result

let eq_classes2 l =
   let a = Array.of_list l in
   (* create an array for storing the classes *)
   let classes = Array.make (Array.length a) [] in
   (* going backwards in the array to have nicer printing *)
   for i = (Array.length classes) - 1 downto 0
   do classes.(a.(i)) <- i :: (classes.(a.(i))) done;
   Array.to_list classes
     (*
   (* And now the printing *)
   Array.iter (
       function
       | [] -> ()
       | h::t ->
          Printf.printf "{%d%a}" h
                        (fun c -> List.iter (fun x -> Printf.fprintf c ",%i" x)) t
     ) classes*)
              
let component2 l =
  let result = ref [] in
  match l with
  | [] -> []
  | x :: tl  ->
     List.iter (fun y ->
                let u = union_l x y l in
                result := eq_classes2 u :: !result;
               ) tl;
     !result

(**********************************************************************************)
(*TEST*)

open Output

let l = [0;1;2;3]
let a = Array.init 3 (fun i -> i)

let print_component =
  print_string "1) component: \n";
  let c = component' l in
  print_list_list c

let print_component2 =
  print_string "\n2) component2: \n";
  let c = List.flatten (component2 l) in
  print_list_list c

let print_union_classes2 =
  let u = union_list l in
  print_string "\n2) union_list: ";
  let _ = print_list u in
  let a = Array.of_list u in
  print_string "\neq_classes_array: ";
  let () = eq_classes a in
  ()

let print_union_classes' =
  let u = union_list l in
  print_string "\n3) union_list: ";
  let _ = print_list u in
  print_string "\neq_classes_list: ";
  let () = eq_classes' u in
  ()

let print_eq_classes_union =
  print_string "\n4) eq_classes_union: ";
  let () = eq_classes_union l in
  ()

let print_union_find =
  print_string "\n5) List: ";
  print_list l; print_string "\n";
  let l = union_l 0 1 l in
  print_string "Result union (0, 1): ";
  print_list l

let print_union_list =
  print_string "\n6) List: ";
  print_list l; print_string "\n";
  print_string "Result: ";
  let l = union_list l in
  print_list l; print_string "\n"
