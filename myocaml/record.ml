(**********************************************************************************)
(* Record *)
type record = {name : string;
               mutable pals : string list}

let record = {name = "Jason";
               pals = ["Norbert"; "Rhys"; "Phineas"]}

let byname = Hashtbl.create 0

let () =
  (* store record *)
  Hashtbl.replace byname record.name record;
  (*give jason a new pal *)
  let jason = Hashtbl.find byname "Jason" in
  jason.pals <- "Theodore" :: jason.pals;
  Printf.printf "Jason now has %d pals \n" (List.length jason.pals)

