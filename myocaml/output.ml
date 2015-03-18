(****************************************************************************************)
(* 08-03-2015                                                                           *)
(* Printing function                                                                    *)
(****************************************************************************************)

(*let print_list = List.iter (fun i -> Printf.printf "%i " i)*)

let rec print_list l =
  match l with
    | [] -> print_string "empty_list"
    | h :: [] -> print_string ""; print_int h; print_string " "
    | h :: tl ->
      let _ = print_string " "; print_int h; print_string "; " in
      print_list tl

let rec print_list_list ls =
  match ls with
  | [] -> ()
  | is :: ls' ->
     let _ = print_string "["; List.iter (fun i -> Printf.printf " %i " i) is in
     print_string "]";
     print_list_list ls'
		         
let rec print_pair pair =
  match pair with
  | [] -> ()
  | (p, i) :: ls ->
     let _ = print_string "\nList:[ "; print_list p; print_string "]\nLength: ";
	     print_int i; print_string "\n"
     in
     print_pair ls
                
let rec print_pair_list l =
  match l with
  | [] -> ()
  | h :: tl ->
     let _ = print_pair h in
     print_pair_list tl
                     
let rec print_pair_list2 (p,l) =
  print_list p;
  print_string "; ";
  print_list l
             
let print_int_pair (p,i) =
  print_int p; print_string ","; print_int i; print_string "\n"
                                   
