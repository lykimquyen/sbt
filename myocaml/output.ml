(****************************************************************************************)
(* 08-03-2015                                                                           *)
(* Printing function                                                                    *)
(****************************************************************************************)

let print_list = List.iter (fun i -> Printf.printf "%i " i)

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
                                   
