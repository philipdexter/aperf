let for_loops = ref []

let input_files = ref []

let eval_file = ref None
let build_command = ref None
let perfs = ref None

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
    default_mapper.expr mapper e
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
    | (loc,payload) :: attrs' ->
      if String.equal "perforate" loc.txt then
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
  let open Ast_helper in
  let open Ast_mapper in
  let attributes = e.pexp_attributes in
  match e.pexp_desc with
  | Pexp_for (p, start, bound, dir, body) ->
    let this_config = List.hd !active_config in
    active_config := List.tl !active_config ;
    let do_perforation = this_config <> 1. in
    if do_perforation then
      begin
        let (pexp_attributes, _perforation) = perforate_property attributes in
        let perforation = match _perforation with Some f -> f | None -> 0.8 in
        let perforation = this_config in
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
             start
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
  let results_out = open_out "results.data" in
  Printf.fprintf results_out "# config time accuracy\n" ;
  let rec perforations list = function
    | 0 -> []
    | 1 -> List.map (fun x -> [x]) list
    | len ->
      let rest = perforations list (len-1) in
      List.concat @@ List.map (fun xs -> List.map (fun x -> x :: xs) list) rest in
  let perfs' = match !perfs with None -> [0.25 ; 0.50 ; 0.75 ; 1.] | Some p -> p in
  let basic_configs len = perforations perfs' len in
  basic_configs (List.length !for_loops) |> List.iter (fun config ->
      let used_config = config in
      active_config := config ;
      Printf.printf ">>>>\n> running with config:\n> %s\n" (String.concat "-" (List.map string_of_float !active_config)) ;
      let ast' =
        let open Ast_mapper in
        active_mapper.structure active_mapper ast in
      (* Pprintast.structure Format.std_formatter ast' ; *)
      let fout = Filename.temp_file ~temp_dir:"./tmp/" "perf" ".ml" in
      let fout_native = String.sub fout 0 (String.length fout - 3) ^ ".native" in
      let fn = open_out fout in
      Printf.fprintf fn "%s\n" (Pprintast.string_of_structure ast') ;
      close_out fn ;

      let run command args =
        let (pr0, pw0) = Unix.pipe () in
        let (pr1, pw1) = Unix.pipe () in
        let (pr2, pw2) = Unix.pipe () in
        let _pid = Unix.create_process command (Array.append [| command |] args) pr0 pw1 pw2 in
        Unix.close pw0 ;
        Unix.close pw1 ;
        Unix.close pw2 ;
        let echo_out = Unix.in_channel_of_descr pr1 in
        let echo_stderr = Unix.in_channel_of_descr pr2 in
        let stdout_lines = ref [] in
        (try
           while true do
             stdout_lines := input_line echo_out :: !stdout_lines
           done
         with
           End_of_file -> close_in echo_out) ;
        let stderr_lines = ref [] in
        (try
           while true do
             stderr_lines := input_line echo_stderr :: !stderr_lines
           done
         with
           End_of_file -> close_in echo_stderr) ;

        ignore @@ Unix.waitpid [] _pid ;

        List.rev !stdout_lines, List.rev !stderr_lines in


      let print_both (a, b) =
        print_endline "> stdout" ;
        List.iter print_endline a ;
        print_endline "> stderr" ;
        List.iter print_endline b in

      print_endline "> building..." ;
      print_both @@ (match !build_command with
      | None -> run "ocamlfind" [| "ocamlopt" ; fout ; "-o" ; fout_native |]
      | Some bc ->
        (match Str.split (Str.regexp " ") bc with
         | [] -> failwith ("error: bad command: " ^ bc)
         | command :: args -> run command (Array.of_list (args @ [ fout ; fout_native])))) ;

      print_endline "> running..." ;
      let start_time = Unix.gettimeofday () in
      print_both @@ run fout_native [||] ;
      Printf.printf "> elapsed time: %f sec\n" (Unix.gettimeofday () -. start_time) ;

      print_endline "> evaluating..." ;

      let fitness = match !eval_file with
        | None -> (Printf.printf "> no eval file, stopping here\n" ; 0.)
        | Some file ->
          begin
            let fitness =
              let stdout, stderr = run file [| fout_native |] in
              match stdout with
              | [fs] -> (try (abs_float (float_of_string fs)) with _ -> failwith (String.concat "" stdout))
              | _ -> failwith (String.concat "" stdout) in
            Printf.printf "> fitness: %f\n" fitness ;
            fitness
          end in

        Printf.fprintf results_out "%s %s %f %f\n" (String.concat "-" (List.map string_of_float used_config)) fout_native total_time fitness) ;
  close_out results_out

let set_eval_file s =
  eval_file := Some s

let set_build_command s =
  build_command := Some s

let set_perfs s =
  perfs := Some (List.map float_of_string (Str.split (Str.regexp ",") s))

let anon_arg name =
  if Filename.check_suffix name ".ml" then
    input_files := name :: !input_files
  else
    failwith ("unsupported file "^name)

let args =
  let open Arg in
  align [
  "--eval", String set_eval_file, "set the eval file" ;
  "--build", String set_build_command, "set the build command" ;
  "--perfs", String set_perfs, "set the perforations" ;
  ]

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

       Printf.printf "> found %d for loops for perforation\n" (List.length !for_loops) ;

       try_perforation pstr' ;

       Format.pp_print_newline fmt ())
