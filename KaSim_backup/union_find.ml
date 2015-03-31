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

 let print_t parameter error set =
  Stochastic_classes_type.AgentMap.print
    error
    (fun error parameter l ->
     let _ =
       print_string "\nSET:"; print_list l;
       print_string "\n"     
     in
     error
    ) parameter set
    
let print_set set =
  print_list (Stochastic_classes_type.Set_list_union.elements set);
  print_string "\n"

 (*let print_remanent_dic parameter error dic =
  Stochastic_classes_type.Dictionary_of_Stochastic_classes.print
    parameter
    error
    (fun parameter error elt l _ _ ->
      let _ = Printf.printf "Stochastic_class_id:%i:" elt in
      let _ =
        print_string "site_type:{";
        let rec print_list l =
          match l with
            | [] -> ()
            | h :: [] -> print_int h; print_string "}"
            | h :: tl ->
              let _ = print_int h; print_string "," in
              print_list tl in
        print_list l
      in
      let _ = print_newline () in
      error
    ) dic.Stochastic_classes_type.dic_union*)
                          
let print_remanent_pointer parameter error pointer =
  Int_storage.Nearly_inf_Imperatif.print
          error
          (fun error parameter p ->
           let _ = print_set p in
           let _ = print_newline() in
           error) parameter pointer
          
(*let get_id_for_value parameter error t set =
  match Int_storage.Nearly_inf_Imperatif.unsafe_get parameter error t set with
    | error, None -> error, 
      Stochastic_classes_type.Set_list_union.empty_set
    | error, Some ids -> error, ids*)

let get_id_for_value parameter error t set =
  match Int_storage.Nearly_inf_Imperatif.unsafe_get parameter error t set with
    | error, None -> error, Stochastic_classes_type.Set_list_union.empty_map
    | error, Some ids -> error, ids
      
(*let store_pointer_backward parameter error id pointer_backward a =
  Array.fold_left
    (fun (error,pointer_backward) elt ->
      let error, old_set_id =
	get_id_for_value parameter error elt pointer_backward
      in
      let error, new_set_id =
        Stochastic_classes_type.Set_list_union.add_set
          parameter error id old_set_id
      in
      Int_storage.Nearly_inf_Imperatif.set
        parameter
        error
        elt
        new_set_id
        pointer_backward)
    (error, pointer_backward)
    a*)

let store_pointer_backward parameter error id pointer_backward a =
  Array.fold_left
    (fun (error,pointer_backward) elt ->
      let error, old_set_id =
	get_id_for_value parameter error elt pointer_backward
      in
      let error, new_set_id =
        Stochastic_classes_type.Set_list_union.add_map
          parameter error elt id old_set_id
      in
      Int_storage.Nearly_inf_Imperatif.set
        parameter
        error
        elt
        new_set_id
        pointer_backward)
    (error, pointer_backward)
    a

let store_new_class parameter error a remanent =
  (*the current remanent information: dictionary, pointer_backward*)
  let good_lists = remanent.Stochastic_classes_type.dic_union in
  let pointer_backward = remanent.Stochastic_classes_type.pointer in
  (*match l with
  | [] -> error, remanent
  | _ ->*)
     (*get allocate_id from a dictionary*)
     let error, output =
       Stochastic_classes_type.Dictionary_of_Stochastic_classes.allocate
         parameter
         error
         Misc_sa.compare_unit
         a (*union_list?*)
         ()
         Misc_sa.const_unit
         good_lists
     in
     let error,(id,dic) =
       match output with
       |Some (al,_,_,dic) -> error,(al,dic)
       | None -> warn
            parameter
            error
            (Some "line 106")
            Exit
            (0,good_lists)
     in
     let error,pointer_backward =
       store_pointer_backward parameter error id pointer_backward a
     in
     error, {
       Stochastic_classes_type.dic_union = dic; 
       Stochastic_classes_type.pointer = pointer_backward}

let empty_remanent parameter error =
  let init_dic = Stochastic_classes_type.Dictionary_of_Stochastic_classes.init()in
  let error, init_pointer = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in
  let empty =
    {Stochastic_classes_type.dic_union = init_dic;
     Stochastic_classes_type.pointer = init_pointer}
  in empty

(*let eq_classes_dic parameter error a =
  let dic = Stochastic_classes_type.Dictionary_of_Stochastic_classes.init()in
  let error, pointer = Int_storage.Nearly_inf_Imperatif.create parameter error 0 in 
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
  let pointer, a = aux (size - 1) (pointer, a) in
  pointer, a*)

let eq_classes_dic2 parameter error a =
  let dic = Stochastic_classes_type.Dictionary_of_Stochastic_classes.init()in
  let error, pointer = error, Stochastic_classes_type.Set_list_union.empty_map in 
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
        Stochastic_classes_type.Set_list_union.find_map_option
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
      let error, classes = Stochastic_classes_type.Set_list_union.add_map parameter error
        rep (k :: get_rep) classes
      in
      aux (k - 1) (classes, union_list)
  in
  let pointer, a = aux (size - 1) (pointer, a) in
  pointer, a

let union_dic parameter error classes =
  List.fold_left (fun (error, acc) l ->
                  let size = List.length l in
                  let a = Array.init size (fun i -> i) in
                  match l with
                  | [] | [_] -> error, acc
                  | t :: q ->
                    let pointer = acc.Stochastic_classes_type.pointer in
                    let error, potential_supersets =
                      get_id_for_value parameter error t pointer in
                    let rec aux to_visit =
                      match to_visit with
                        | [] -> error, acc
                        | t' :: q' ->
                          let a = union t t' a in
                          (*get t and t' in a *)
                          (*dic*)
                          let (dic,a) = eq_classes_dic2 parameter error a in
                          store_new_class parameter error a acc;
                          print_string "dic:";
                          Stochastic_classes_type.Set_list_union.iter_map
                            (fun k l ->
                              print_string "k: ";
                              print_int k; print_string "\n";
                              print_string "list: ";
                              print_list l; print_string "\n"
                            ) dic;
                          (*let (dic,a) = eq_classes_dic parameter error a in
                          store_new_class parameter error a acc;
                          print_string "dic:";
                          Int_storage.Nearly_inf_Imperatif.print
                            error (fun error parameter l ->
                              let _ = print_list l;
                                print_string "\n" in
                              error) parameter dic;*)
                          aux q'
                    in aux q)
    (error, empty_remanent parameter error) classes             
    
