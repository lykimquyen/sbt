let l = [1; 2; 3]
          
let test = List.rev (List.fold_right (fun x acc -> [x]@acc) l [])

let print_test = List.iter (Printf.printf "%i\n") test

let test_left = List.fold_left (fun x acc -> x + acc) 0 l

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

(*compute the length of the int list list*)
let length_list classes =
  let rec aux classes size =
    match classes with
      | [] -> size
      | c :: classes' -> aux classes' (size + 1)
  in aux classes 0

let length_list_list classes = List.map length_list classes

(*sorting the length of a list of list *)

let length_sort lists =
  let lists = List.map (fun list -> List.length list, list) lists in
  let lists = List.sort (fun a b -> compare (fst a) (fst b)) lists in
  List.map snd lists

let length_test lists =
  let lists = List.map (fun list -> List.length list, list) lists in
  lists 

(*TEST*)
let ls = [[0;1;2;3]]

let print_test = List.iter (Printf.printf "%i\n")

  


