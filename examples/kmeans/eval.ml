open Point

(* TODO read answers from argument file *)

let _ =
  let answers =
    let fin = open_in "answers.in" in
    let answers = ref [] in
    (try
       while true do
         let line = input_line fin in
         let [x;y] = Str.split (Str.regexp " ") line in
         answers := {x=float_of_string x ; y=float_of_string y} :: !answers ;
       done
     with End_of_file -> ()) ;
    Array.of_list (List.rev !answers) in

  let points = ref [] in
  (try
     while true do
       let line = input_line stdin in
       let [x;y] = Str.split (Str.regexp " ") line in
       points := {x=float_of_string x; y=float_of_string y} :: !points
     done
   with End_of_file -> ()) ;
  if List.length !points <> Array.length answers then
    Printf.printf "1.0\n"
  else
    begin
      List.rev !points |>
      (* 1414.213562373095 *)
      List.mapi (fun i p -> dist answers.(i) p /. 7071.068) |>
      List.fold_left (+.) 0. |>
      fun x -> x /. (float_of_int (Array.length answers)) |>
      Printf.printf "%f\n"
    end

