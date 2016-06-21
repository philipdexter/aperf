let for_loops = ref []

let args =
  let open Arg in
  align []

let input_files = ref []

let anon_arg name =
  if Filename.check_suffix name ".ml" then
    input_files := name :: !input_files

let usage_msg =
  Printf.sprintf
    "Usage: %s\n"
    Sys.argv.(0)

let rec exp_mapper e =
  let open Parsetree in
  let open Location in
  let open Lexing in
  let pexp_desc =
    match e.pexp_desc with
    | Pexp_for (p, start, bound, dir, body) as x -> for_loops := ("hi" ^ (string_of_int e.pexp_loc.loc_start.pos_lnum)) :: !for_loops ; x
    | Pexp_let (rflag, vbs, e) -> Pexp_let (rflag, vbs, exp_mapper e)
    | Pexp_sequence (e1, e2) -> Pexp_sequence (exp_mapper e1, exp_mapper e2)
    | Pexp_extension (loc, payload) ->
      begin
        let payload' =
          match payload with
          | PStr strs -> PStr (List.map structure_mapper strs)
          | _ -> failwith "payload not PStr" in
        Pexp_extension (loc, payload')
      end
    | x -> x in
  { e with pexp_desc }

and structure_mapper si =
  let open Parsetree in
  let open Location in
  let open Lexing in
  let pstr_desc =
    match si.pstr_desc with
    | Pstr_value (rflag, vbs) -> Pstr_value (rflag, List.map (fun vb -> { vb with pvb_expr = exp_mapper vb.pvb_expr }) vbs)
    | Pstr_eval (exp, attr) -> Pstr_eval (exp_mapper exp, attr)
    | x -> x in
  { si with pstr_desc }

let active_config = ref []

exception Backup
let perforate_property attributes =
  let open Location in
  let open Parsetree in
  let rec loop attrs =
    match attrs with
    | [] -> attrs, None
    | (loc,payload) :: attrs' -> if String.equal "perforate" loc.txt
                                 then
                                   try
                                     let i =
                                       match payload with
                                       | PStr [{pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_integer (i,_)) }, _)}] -> int_of_string i
                                       | PStr [] -> raise Backup
                                       | _ -> failwith "bad perforation payload" in
                                     (attrs', Some i)
                                   with Backup -> (attrs', None)
                                 else
                                   match loop attrs' with
                                     (attrs'', r) -> ((loc,payload) :: attrs'', r) in
  loop attributes

let rec active_exp_mapper ?extra_attrs e =
  let open Parsetree in
  let open Location in
  let open Lexing in
  let attributes = e.pexp_attributes @ (match extra_attrs with Some attrs -> attrs | None -> []) in
  let pexp_desc =
    match e.pexp_desc with
    | Pexp_for (p, start, bound, dir, body) ->
      if List.exists (fun (a,_) -> String.equal "perforate" a.txt) attributes then
        begin
          let old_start =
            match start with
            | { pexp_desc = Pexp_constant (Pconst_integer (i,_)) } -> int_of_string i
            | _ -> failwith "start not an integer constant" in
          let old_bound =
            match bound with
            | { pexp_desc = Pexp_constant (Pconst_integer (i,_)) } -> int_of_string i
            | _ -> failwith "bound not an integer constant" in
          let (pexp_attributes, perforation) =
            match perforate_property attributes with
            | (attributes, sn) -> (attributes, match sn with Some n -> n | None -> (old_bound - old_start) / 2) in
          Pexp_for (p, { start with pexp_desc = Pexp_constant (Pconst_integer (string_of_int 0, None)) },
                    { bound with pexp_desc = Pexp_constant (Pconst_integer (string_of_int perforation, None)) }, dir, body)
        end
      else
        Pexp_for (p, start, bound, dir, body)
    | Pexp_let (rflag, vbs, e) -> Pexp_let (rflag, vbs, active_exp_mapper e)
    | Pexp_sequence (e1, e2) -> Pexp_sequence (active_exp_mapper e1, active_exp_mapper e2)
    | Pexp_extension (loc, payload) ->
      begin
        let perforating = String.equal "perforate" loc.txt in
        let bonus_attribute = if perforating then [loc, PStr []] else [] in
        let payload' =
          match payload with
          | PStr strs -> PStr (List.map (fun str -> (active_structure_mapper ~extra_attrs:(attributes @ bonus_attribute)) str) strs)
          | _ -> failwith "payload not PStr" in
        if perforating then
          begin
            let expressions = match payload' with
              | PStr strs ->
                strs |> List.map (fun str ->
                  match str.pstr_desc with
                  | Pstr_eval (e, _) -> e
                  | _ -> failwith "payload structure not Pstr_eval")
              | _ -> failwith "payload not PStr" in
            let rec loop = function
              | [] -> Printast.expression 0 Format.std_formatter e ; failwith "empty expression"
              | x :: xs -> { pexp_desc = Pexp_sequence (x, if xs = [] then { pexp_desc = Pexp_construct ({ txt = Longident.Lident "()" ; loc = Location.none }, None) ; pexp_loc = x.pexp_loc ; pexp_attributes = []} else (loop xs))
                           ; pexp_loc = x.pexp_loc
                           ; pexp_attributes = x.pexp_attributes } in
            (loop expressions).pexp_desc
          end
        else
          Pexp_extension (loc, payload')
      end
    | x -> x in
  { e with pexp_desc }

and active_structure_mapper ?extra_attrs si =
  let open Parsetree in
  let open Location in
  let open Lexing in
  let pstr_desc =
    match si.pstr_desc with
    | Pstr_value (rflag, vbs) -> Pstr_value (rflag, List.map (fun vb -> { vb with pvb_expr = active_exp_mapper ?extra_attrs vb.pvb_expr }) vbs)
    | Pstr_eval (exp, attr) -> Pstr_eval (active_exp_mapper ?extra_attrs exp, attr)
    | x -> x in
  { si with pstr_desc }

let try_perforation ast =
  let open Unix in
  active_config := [true, false, false, false, false] ;
  let ast' = List.map active_structure_mapper ast in
  (* Pprintast.structure Format.std_formatter ast' ; *)
  let fout = Filename.temp_file ~temp_dir:"./" "perf" ".ml" in
  let fn = open_out fout in
  Printf.fprintf fn "%s\n" (Pprintast.string_of_structure ast') ;
  close_out fn ;
  let (pr0, pw0) = Unix.pipe () in
  let (pr1, pw1) = Unix.pipe () in
  let (pr2, pw2) = Unix.pipe () in
  let pid = Unix.create_process "ocaml" [| "ocaml"; fout |] pr0 pw1 pw2 in
  Unix.close pw0 ;
  Unix.close pw1 ;
  Unix.close pw2 ;
  let echo_out = Unix.in_channel_of_descr pr1 in
  let echo_stderr = Unix.in_channel_of_descr pr2 in
  let lines = ref [] in
  (try
     while true do
       print_endline (input_line echo_out) ;
     done
   with
     End_of_file -> close_in echo_out) ;
  List.iter print_endline !lines ;
  let lines = ref [] in
  (try
     while true do
       print_endline (input_line echo_stderr) ;
     done
   with
     End_of_file -> close_in echo_stderr) ;
  List.iter print_endline !lines ;
  Unix.kill pid 15

let () =
  Arg.parse args anon_arg usage_msg ;
  print_endline "input files:" ;
  List.iter print_endline !input_files ;
  !input_files |> List.iter
    (fun source ->
       Location.input_name := source ;
       let fmt = Format.std_formatter in
       let lexer = Lexing.from_channel (open_in source) in
       let pstr = Parse.implementation lexer in
       (* Printast.implementation fmt pstr ; *)
       let pstr' = List.map structure_mapper pstr in

       Printf.printf "aperf: Found %d for loops for perforation\n" (List.length !for_loops) ;

       try_perforation pstr' ;

       Format.pp_print_newline fmt ())
