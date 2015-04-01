(**
    * union_find.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2015, the 11th of March
    * Last modification: 
    * * 
    * This library provides primitives to deal with union find algorithm with
    * path compression
    *  
    * Copyright 2010,2011 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    *  under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default =
  Exception.warn parameters mh (Some "Stochastic classes") message exn
                 (fun () -> default)

type union_find =
    {
      treeArr: int array
    }

let create n =
  {
    treeArr = Array.init n (fun i -> i)
  }

(* findSet(e): which return a pointer to the representative of the set
   containing e. Since the set are disjoint, e containted in one set
   only. Therefore, the returned representative can be uniquely determined.
*)
      
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
           path point to the root. And return the root afterwards *)
        pointToRoot parent l;
        parent;
      end
  in
  helper e []

let dump a = Array.iteri (Printf.fprintf stdout "%i:%i") a

let union x y a =
  let root_x = findSet x a in
  let root_y = findSet y a in
  let _ = a.(root_x) <- root_y in
  (*let _ = print_string "dump: "; dump a; print_string "\n" in*)
  a

 let rec print_list l =
  match l with
  | [] -> print_string "empty"
  | h :: [] ->  print_int h; print_string " "
  | h :: tl ->
     let _ = print_int h; print_string "," in
     print_list tl
       
 let rec print_list_list ls =
  match ls with
  | [] -> ()
  | h :: [] -> print_list h; print_string " "
  | h :: tl ->
     let _ =  print_list h;
              print_string "; " in
     print_list_list tl

let get_id_for_value parameter error t set =
  match Int_storage.Nearly_inf_Imperatif.unsafe_get parameter error t set with
    | error, None -> error, Stochastic_classes_type.Set_list_id.empty_set
    | error, Some ids -> error, ids

let store_pointer parameter error id pointer (l:int list) =
  List.fold_left
    (fun (error, pointer) elt ->
      let error, old_set_id =
        get_id_for_value parameter error elt pointer
      in
      let error, new_set_id =
        Stochastic_classes_type.Set_list_id.add_set
          parameter error id old_set_id
      in
      (*store*)
      Int_storage.Nearly_inf_Imperatif.set
        parameter
        error
        elt
        new_set_id
        pointer)
    (error, pointer) l

let store_new_class parameter error (l: int list) remanent =
  let pair_dic = remanent.Stochastic_classes_type.dic in
  let pointer = remanent.Stochastic_classes_type.pointer in
  (*get allocate from a dictionary*)
  let error, output =
    Stochastic_classes_type.Dictionary_of_Stochastic_classes.allocate
      parameter
      error
      Misc_sa.compare_unit
      l
      ()
      Misc_sa.const_unit
      pair_dic
  in
  let error, (id, dic) =
    match output with
      | Some (al, _, _, dic) -> error, (al, dic)
      | None -> warn parameter error (Some "line 137") Exit (0, pair_dic)
  in
  (*store pointer*)
  let error, pointer =
    store_pointer parameter error id pointer l
  in
  error,
  {
    Stochastic_classes_type.dic = dic;
    Stochastic_classes_type.pointer = pointer
  }

let empty_remanent parameter error =
  let init_dic = Stochastic_classes_type.Dictionary_of_Stochastic_classes.init() in
  let error, init_pointer = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let empty =
    {Stochastic_classes_type.dic = init_dic;
     Stochastic_classes_type.pointer = init_pointer}
  in empty

(*TEST*)
let interation parameter error acc l =
  match l with
    | [] -> error, acc
    | t :: q ->
      let pointer = acc.Stochastic_classes_type.pointer in
         (* get the set of list(id) containing t *)
      let error, potential_supersets =
	get_id_for_value parameter error t pointer
      in
      let rec aux to_visit potential_supersets =
        match to_visit with
	  | []  -> error,acc
	  | t'::q' ->
               (* get the set of list(id) containing t *)
            let error, potential_supersets' =
	      get_id_for_value parameter error t' pointer
            in
               (* intersection of two sets *)
            let error, potential_superset =
              Stochastic_classes_type.Set_list_id.inter
                parameter
                error
                potential_supersets
                potential_supersets'
            in
            if Stochastic_classes_type.Set_list_id.is_empty_set
              potential_superset
            then
              store_new_class parameter error l acc
            else
              aux q' potential_superset
      in
         (*check the beginning state of a superset*)
      if Stochastic_classes_type.Set_list_id.is_empty_set
        potential_supersets
      then
           (*if it is empty then store it to remanent*)
        store_new_class parameter error l acc
      else
        aux q potential_supersets

(*use this after eq_classes_dic*)
let interation_list parameter error classes =
  List.fold_left (fun (error, acc) l ->
    interation parameter error acc l
  ) (error, empty_remanent parameter error) classes
 
let print_remanent parameter error remanent =(*FIXME*)
  Stochastic_classes_type.Dictionary_of_Stochastic_classes.print
    parameter
    error
    (fun parameter error elt l _ _ ->
      let _ = Printf.printf "Stochastic_class_id:%i:" elt in
      let _ =
        print_string "site_type:{";
        print_list l
      in
      let _ = print_string "}"; print_newline () in
      error
    ) remanent.Stochastic_classes_type.dic

let eq_classes_dic parameter error a =
  let error, classes = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let size = Array.length a in
  let rec aux k (classes,union_list) =
    if  k < 0 
    then
      classes, union_list
    else 
      (*find the parent of the union*)
      let rep = findSet k a in
      (*check if inside classes has already has this parent*)
      let error, old =
        Int_storage.Nearly_inf_Imperatif.get
          parameter
          error
          rep
          classes
      in
      let get_rep =
        match old with
          | None -> []
          | Some r -> r
      in
      (*store the result inside classes*)
      let error, classes = Int_storage.Nearly_inf_Imperatif.set parameter error
        rep (k :: get_rep) classes
      in
      aux (k - 1) (classes, union_list)
  in
  let classes, a = aux (size - 1) (classes, a) in
 (* let error, classes =
    Int_storage.Nearly_inf_Imperatif.fold parameter error
      (fun parameter error k l ls ->
        match l with
            | [] -> error, ls
            | _ ->                
              let _ = print_string "LIST: ";
                print_list_list (l::ls); print_string "\n"
              in
              error, l :: ls
      ) classes []
  in*)
  classes, a
    
let union_dic parameter error classes =
  List.fold_left (fun (error, remanent_acc) l ->
    let size = List.length l in
    let a = Array.init size (fun i -> i) in
    match l with
      | [] | [_] -> error, remanent_acc (*FIXME*)
      | t :: q ->
        let rec aux to_visit =
          match to_visit with
            | [] | [_] -> error, remanent_acc
            | t' :: q' ->
              let union_array = union t t' a in
              let (classes, a) = eq_classes_dic parameter error union_array  in
              let  _ = print_string "CLASSES ";
                Int_storage.Nearly_inf_Imperatif.print
                  error
                  (fun error parameter l ->
                    let _ = print_list l; print_string "\n"
                    in error)
                  parameter
                  classes
              in
              (*let error, result = interation_list parameter error classes in
              let _ = 
                print_remanent parameter error result;
                print_string "\n"
              in*)
              aux q'
        in aux q
  )(error, empty_remanent parameter error) classes
