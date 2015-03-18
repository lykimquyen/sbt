(* union find without path compression *)
(*type union_find =
    {id: int array;
     size: int array}

let create_union n =
  {
    id = Array.init n (fun i -> i);
    size = Array.make n 1
  }

(*find parent without path compression*)
let find_parent parent i =
  let rec find j =
    if parent.(j) = j
    then j
    else find parent.(j)
  in find i


(*find parent with path compression: when the root is found, the elements
  are changed to link to it*)

(* for complexity purpose *)
let union_rank {id; _} i j =
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
  (find_parent id i) = (find_parent id j)*)

(**********************************************************************************)
(* union find with path compression with rank (this rank is not useful
   because it is flat after the compression *)

type union_find_rank =
    {
      treeArr: int array;
      rankArr: int array
    }

(* makeSets(x):
   create a new singleton set contaning x only. x is also the representative
   of this set *)

let makeSets_rank n =
  {
    treeArr = Array.init n (fun i -> i);
    rankArr = Array.make n 0
  }

(* findSet(x): which return a pointer to the representative of the set
   containing x. Since the set are disjoint, x containted in one set
   only. Therefore, the returned representative can be uniquely determined.
   e: is the element we need to find. 
   returns the root of the tree that containing x
*)

let findSet_rank e union_find =
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
        parent
      end
  in
  helper e []

let union_rank x y union_find =
  let root_x = findSet_rank x union_find in
  let root_y = findSet_rank y union_find in
  let treeArr = union_find.treeArr in
  let rankArr = union_find.rankArr in
  let rank_x = rankArr.(root_x) in
  let rank_y = rankArr.(root_y) in
  (*root_y -> root_x*)
  if rank_x > rank_y
  then
    treeArr.(root_y) <- root_x
  else
    if rank_x < rank_y
    then
      treeArr.(root_x) <- root_y
    else
      begin
        treeArr.(root_x) <- root_y;
        rankArr.(root_y) <- (rank_y + 1) 
      end

let is_equivalence_rank x y union_find =
  (findSet_rank x union_find) = (findSet_rank y union_find)
