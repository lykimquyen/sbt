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

(* for complexity purpose *)
let union_rank {id; size} i j =
  let parent_i = find_parent id i in
  let parent_j = find_parent id j in
  (* i and j are not already in the same set, merge them. *)
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

(* checking two subsets is the same or not *)
let is_connected_ranked {id; _} i j =
  (find_parent id i) = (find_parent id j)

(**********************************************************************************)
(*TEST*)


