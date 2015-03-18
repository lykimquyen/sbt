(**********************************************************************************)
(* persistent arrays; see the module [Parray] for explanations *)
(*
module Pa = struct

  type t = data ref
  and data =
    | Array of int array 
    | Diff of int * int * t
        
  let create n v = ref (Array (Array.make n v))
  let init n f = ref (Array (Array.init n f))
    
  (* reroot t ensures that t becomes an Array node *)
  let rec reroot t = match !t with
    | Array _ -> ()
    | Diff (i, v, t') -> 
        reroot t';
        begin match !t' with
          | Array a as n ->
              let v' = a.(i) in
              a.(i) <- v;
              t := n;
              t' := Diff (i, v', t)
          | Diff _ -> assert false
        end
  
  let rec rerootk t k = match !t with
    | Array _ -> k ()
    | Diff (i, v, t') -> 
        rerootk t' (fun () -> begin match !t' with
                      | Array a as n ->
                          let v' = a.(i) in
                          a.(i) <- v;
                          t := n;
                          t' := Diff (i, v', t)
                      | Diff _ -> assert false end; k())

  let reroot t = rerootk t (fun () -> ())

  let rec get t i = match !t with
    | Array a -> 
        a.(i)
    | Diff _ -> 
        reroot t; 
        begin match !t with Array a -> a.(i) | Diff _ -> assert false end
      
  let set t i v = 
    reroot t;
    match !t with
      | Array a as n ->
          let old = a.(i) in
          if old == v then
            t
          else begin
            a.(i) <- v;
            let res = ref n in
            t := Diff (i, old, res);
            res
          end
      | Diff _ ->
          assert false

  let rec print_t t =
    match !t with
      | Array l -> Array.iter (fun i -> print_int i; print_string " ") l
      | Diff (_,_,t')-> print_t t'

end

(* Tarjan's algorithm *)

type t = { 
  mutable father: Pa.t; (* mutable to allow path compression *)
  (*c: Pa.t; (* ranks *)*)
}
      
let create n = 
  { (*c = Pa.create n 0;*)
    father = Pa.init n (fun i -> i) }
    
let rec find_aux f i = 
  let fi = Pa.get f i in
  if fi == i then 
    f, i
  else 
    let f, r = find_aux f fi in 
    let f = Pa.set f i r in
    f, r
      
let find h x = 
  let f,rx = find_aux h.father x in h.father <- f; rx
  
let union h x y = 
  let rx = find h x in
  let ry = find h y in
  if rx != ry then begin
    (*let rxc = Pa.get h.c rx in
      let ryc = Pa.get h.c ry in     *)
    { father = Pa.set h.father ry rx }

    (*if rxc > ryc then
      { h with father = Pa.set h.father ry rx }
    else if rxc < ryc then
      { h with father = Pa.set h.father rx ry }
    else
      { c = Pa.set h.c rx (rxc + 1);
        father = Pa.set h.father ry rx }*)
  end else
    h

(* tests *)
let t = create 10

let print_union {father} =
  Pa.print_t father

let print_test =
  print_string "t: ";
  print_union t;
  print_string "\n";
  let t = union t 0 1 in
  print_string "union 0 1: ";
  print_union t;
  print_string "\n";
  let t1 = union t 2 3 in
  print_string "union 2 3: ";
  print_union t1;
  print_string "\n";*)

(***
let t = create 10
let () = assert (find t 0 <> find t 1)
let t = union t 0 1
let () = assert (find t 0 = find t 1)
let () = assert (find t 0 <> find t 2)
let t = union t 2 3 
let t = union t 0 3
let () = assert (find t 1 = find t 2)
let t = union t 4 4
let () = assert (find t 4 <> find t 3)
***)

(**********************************************************************************)
(*module*)
(*
module type Storage =
  sig
    type union_find = data ref
     and data = Array of int array
              | Node_Array of int * int * union_find

    val create : int -> int -> data ref
    val init : int -> (int -> int) -> data ref
    val rerootk : union_find -> (unit -> 'a) -> 'a
    val reroot : union_find -> unit
    val get : union_find -> int -> int
    val set : union_find -> int -> int -> union_find
    val print_array : union_find -> unit

  end*)
  
module UnionFind = (struct

  type t = data ref
  and data =
    | Array of int array 
    | Node_Array of int * int * t
        
  let create n v = ref (Array (Array.make n v))
  let init n f = ref (Array (Array.init n f))
    
  (* reroot t ensures that t becomes an Array node *)
  let rec reroot t = match !t with
    | Array _ -> ()
    | Node_Array (i, v, t') -> 
        reroot t';
        begin match !t' with
          | Array a as n ->
              let v' = a.(i) in
              a.(i) <- v;
              t := n;
              t' := Node_Array (i, v', t)
          | Node_Array _ -> assert false
        end
  
  let rec rerootk t k = match !t with
    | Array _ -> k ()
    | Node_Array (i, v, t') -> 
       rerootk t' (fun () ->
                   begin match !t' with
                         | Array a as n ->
                            let v' = a.(i) in
                            a.(i) <- v;
                            t := n;
                            t' := Node_Array (i, v', t)
                         | Node_Array _ -> assert false end; k())

  let reroot t = rerootk t (fun () -> ())

  let rec get t i = match !t with
    | Array a -> 
        a.(i)
    | Node_Array _ -> 
        reroot t; 
        begin
          match !t with
          | Array a -> a.(i)
          | Node_Array _ -> assert false
        end
          
  let set t i v = 
    reroot t;
    match !t with
      | Array a as n ->
          let old = a.(i) in
          if old == v then
            t
          else begin
            a.(i) <- v;
            let res = ref n in
            t := Node_Array (i, old, res);
            res
          end
      | Node_Array _ -> assert false

  let rec print_array t =
    match !t with
      | Array l -> Array.iter (fun i -> print_int i; print_string " ") l
      | Node_Array (_,_,t')-> print_array t'

end)

(**********************************************************************************)
(* union find *)

type ufind = { 
  mutable father: UnionFind.t; (* mutable to allow path compression *)
  }
      
let create n = 
  {father = UnionFind.init n (fun i -> i) }

let rec find_aux f i = 
  let fi = UnionFind.get f i in
  if fi == i then 
    f, i
  else 
    let f, r = find_aux f fi in 
    let f = UnionFind.set f i r in
    f, r
      
let find h x = 
  let f, rx = find_aux h.father x in
  h.father <- f;
  rx
  
let union h x y = 
  let rx = find h x in
  let ry = find h y in
  if rx != ry
  then
    { father = UnionFind.set h.father ry rx }
   else
    h

let print_union {father} =
  UnionFind.print_array father

(**********************************************************************************)
(* TEST *)

let l = create 10
(* l: 0 1 2 3 4 5 6 7 8 9 *)
let l1 = create 2

let print_test =
  print_string "l: ";
  print_union l;
  print_string "\n";
  let t = union l 0 1 in
  print_string "union 0 1: ";
  print_union t;
  print_string "\n";
  let t1 = union t 2 3 in
  print_string "union 2 3: ";
  print_union t1;
  print_string "\n"

let empty_ufind = create 0

let print_empty_ufind =
  print_string "create 0:";
  print_union empty_ufind;
  print_string "\n"

let new_eq_class ls =
  match ls with
    | [] | [_] -> empty_ufind
    | t :: q ->
      let new_list = List.fold_left
        (fun union_c t' -> union union_c t t') l1 q in
      new_list

let ls = [0;1]

let print_new_eq_class =
  print_string "union with list:";
  print_list ls;

  let u = new_eq_class ls in
  print_string "\nThe result after union 'new_eq_class': ";
  print_union u; print_string "\n"


