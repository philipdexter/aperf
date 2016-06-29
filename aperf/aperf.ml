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

let exp_mapper mapper e =
  let open Parsetree in
  let open Location in
  let open Lexing in
  let open Ast_mapper in
  match e.pexp_desc with
  | Pexp_for (p, start, bound, dir, body) ->
    for_loops := ("hi" ^ (string_of_int e.pexp_loc.loc_start.pos_lnum)) :: !for_loops ;
    e
  | x -> default_mapper.expr mapper e

let search_mapper =
  let open Parsetree in
  let open Ast_mapper in
  let open Location in
  { default_mapper with
    expr = (fun mapper expr ->
        match expr with
        | { pexp_desc = Pexp_extension ({ txt = "perforate" } as loc, PStr [{pstr_desc = Pstr_eval (e,attributes)} as struc])} ->
          { expr with
            pexp_desc = Pexp_extension (loc,
                                        PStr [{ struc with
                                                pstr_desc = Pstr_eval (exp_mapper mapper e, attributes) }]) }
        | { pexp_attributes = attr} ->
          if List.exists (fun (a,_) -> String.equal "perforate" a.txt) attr then
            exp_mapper mapper expr
          else
            default_mapper.expr mapper expr) }

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
                                       | PStr [{pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_float (f,_)) }, _)}] -> float_of_string f
                                       | PStr [] -> raise Backup
                                       | _ -> failwith "bad perforation payload" in
                                     (attrs', Some i)
                                   with Backup -> (attrs', None)
                                 else
                                   match loop attrs' with
                                     (attrs'', r) -> ((loc,payload) :: attrs'', r) in
  loop attributes

let active_exp_mapper mapper e =
  let open Parsetree in
  let open Location in
  let open Lexing in
  let open Ast_helper in
  let open Ast_mapper in
  let attributes = e.pexp_attributes in
  match e.pexp_desc with
  | Pexp_for (p, start, bound, dir, body) ->
    let do_perforation = List.hd !active_config in
    active_config := List.tl !active_config ;
    if do_perforation then
      begin
        let (pexp_attributes, _perforation) = perforate_property attributes in
        let perforation = match _perforation with Some f -> f | None -> 0.8 in
        let this_of_that_of_expr this that expr = Exp.apply (Exp.ident { txt = Longident.Lident (this^"_of_"^that) ; loc = !default_loc }) [(Asttypes.Nolabel,expr)] in
        let float_of_int_of_expr = this_of_that_of_expr "float" "int" in
        let int_of_float_of_expr = this_of_that_of_expr "int" "float" in
        let bound_minus_start = Exp.apply (Exp.ident { txt = Longident.Lident "-" ; loc = !default_loc })
            [Asttypes.Nolabel, bound ; Asttypes.Nolabel, start] in
        let new_relative_bound = int_of_float_of_expr @@ Exp.apply (Exp.ident { txt = Longident.Lident "*." ; loc = !default_loc })
            [Asttypes.Nolabel,float_of_int_of_expr  bound_minus_start ; Asttypes.Nolabel,Exp.constant (Const.float (string_of_float perforation)) ] in
        let new_absolute_bound = Exp.apply (Exp.ident { txt = Longident.Lident "+" ; loc = !default_loc })
            [Asttypes.Nolabel, start ; Asttypes.Nolabel, new_relative_bound] in
        mapper.expr mapper
        { (Exp.for_ p
             { start with pexp_desc = Pexp_constant (Pconst_integer (string_of_int 0, None)) }
             new_absolute_bound
             dir
             body)
          with pexp_attributes }
      end
    else
      default_mapper.expr mapper e
  | x -> default_mapper.expr mapper e

let active_mapper =
  let open Parsetree in
  let open Ast_mapper in
  let open Location in
  { default_mapper with
    expr = (fun mapper expr ->
        match expr with
        | { pexp_desc = Pexp_extension ({ txt = "perforate" }, PStr [{pstr_desc = Pstr_eval (e,attributes)}])} -> active_exp_mapper mapper e
        | { pexp_attributes = attr} ->
          if List.exists (fun (a,_) -> String.equal "perforate" a.txt) attr then
            active_exp_mapper mapper expr
          else
            default_mapper.expr mapper expr) }

let try_perforation ast =
  let open Unix in
  let rec replicate i e = if i == 0 then [] else e :: replicate (i-1) e in
  let rec count_to i = let rec loop k = if k > i then [] else k :: loop (k+1) in loop 0 in
  let basic_configs len = [replicate len false] @ List.map (fun i -> replicate i false @ [true] @ replicate (len-1-i) false) (count_to (len-1)) in
  basic_configs (List.length !for_loops) |> List.iter (fun config ->
      active_config := config ;
      print_endline "running" ;
      List.iter (Printf.printf "%b - ") !active_config ;
      print_endline "\n----" ;
      let ast' =
        let open Ast_mapper in
        active_mapper.structure active_mapper ast in
      (* Pprintast.structure Format.std_formatter ast' ; *)
      let fout = Filename.temp_file ~temp_dir:"./tmp/" "perf" ".ml" in
      let fout_native = String.sub fout 0 (String.length fout - 3) ^ ".native" in
      let fn = open_out fout in
      Printf.fprintf fn "%s\n" (Pprintast.string_of_structure ast') ;
      close_out fn ;


      let run_and_print command args =
        let (pr0, pw0) = Unix.pipe () in
        let (pr1, pw1) = Unix.pipe () in
        let (pr2, pw2) = Unix.pipe () in
        let _pid = Unix.create_process command (Array.append [| command |] args) pr0 pw1 pw2 in
        Unix.close pw0 ;
        Unix.close pw1 ;
        Unix.close pw2 ;
        let echo_out = Unix.in_channel_of_descr pr1 in
        let echo_stderr = Unix.in_channel_of_descr pr2 in
        (try
           while true do
             print_endline (input_line echo_out) ;
           done
         with
           End_of_file -> close_in echo_out) ;
        (try
           while true do
             print_endline (input_line echo_stderr) ;
           done
         with
           End_of_file -> close_in echo_stderr) ;

        ignore @@ Unix.waitpid [] _pid in


      print_endline "building..." ;
      run_and_print "ocamlopt" [| fout ; "-o" ; fout_native |] ;

      print_endline "running..." ;
      let start_time = Unix.gettimeofday () in
      run_and_print fout_native [||] ;
      Printf.printf "elapsed time: %f sec\n" (Unix.gettimeofday () -. start_time)
    )

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

       let open Ast_mapper in
       let pstr' = search_mapper.structure search_mapper pstr in

       Printf.printf "aperf: Found %d for loops for perforation\n" (List.length !for_loops) ;

       try_perforation pstr' ;

       Format.pp_print_newline fmt ())
