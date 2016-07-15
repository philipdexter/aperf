let rec replicate e = function
  | 0 -> []
  | n -> e :: replicate e (n-1)

let for_loops = ref []

let input_files = ref []

let eval_file = ref None
let build_command = ref None
let perfs = ref None
let auto_explore = ref false
let accuracy_bound = ref 0.02

let usage_msg =
  Printf.sprintf
    "Usage: %s\n"
    Sys.argv.(0)

let score speedup accuracy b = 2. /. ( (1. /. (speedup -. 1.)) +. (1. /. (0.05 -. (accuracy /. b))) )

let search_exp_mapper mapper e =
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
                                                pstr_desc = Pstr_eval (search_exp_mapper mapper e, attributes) }]) }
        | { pexp_attributes = attr} ->
          if List.exists (fun (a,_) -> String.equal "perforate" a.txt) attr then
            search_exp_mapper mapper expr
          else
            default_mapper.expr mapper expr) }

let active_config = ref []

let active_exp_mapper mapper e =
  let open Parsetree in
  let open Location in
  let open Ast_helper in
  let open Ast_mapper in
  match e.pexp_desc with
  | Pexp_for (p, start, bound, dir, body) ->
    let this_config = List.hd !active_config in
    active_config := List.tl !active_config ;
    let do_perforation = this_config <> 1. in
    if do_perforation then
      begin
        let used_var = match p with { ppat_desc = Ppat_var { txt = var } } -> var | _ -> assert false in
        let ident i = Exp.ident { txt = Longident.Lident i ; loc = !default_loc } in
        let apply func args = Exp.apply (ident func) (List.map (fun e -> Asttypes.Nolabel, e) args) in
        let perforation = this_config in
        let this_of_that_of_expr this that expr = apply (this^"_of_"^that) [expr] in
        let float_of_int_of_expr = this_of_that_of_expr "float" "int" in
        let int_of_float_of_expr = this_of_that_of_expr "int" "float" in
        let bound_minus_start = apply "-" [bound ; start] in
        let new_relative_bound = int_of_float_of_expr @@ apply "*." [float_of_int_of_expr  bound_minus_start ; Exp.constant (Const.float (string_of_float perforation)) ] in
        let new_absolute_bound = apply "+" [start ; new_relative_bound] in
        let skip_every = apply "/." [float_of_int_of_expr bound_minus_start ; apply "-." [float_of_int_of_expr bound_minus_start ; apply "*." [float_of_int_of_expr bound_minus_start ; Exp.constant (Const.float (string_of_float perforation))]]] in
        if perforation > 0.5 then
          (* skip elements *)
          mapper.expr mapper
            (Exp.let_ Asttypes.Nonrecursive [{ pvb_pat = Pat.var { txt = used_var ; loc = !default_loc }  ; pvb_expr = apply "ref" [start] ; pvb_loc = !default_loc ; pvb_attributes = []}]
               (Exp.while_ (apply "<" [ apply "!" [ident used_var] ; bound])
                  (Exp.let_ Asttypes.Nonrecursive [{ pvb_pat = Pat.var { txt = "old_i" ; loc = !default_loc } ; pvb_expr = apply "!" [ident used_var] ; pvb_loc = !default_loc ; pvb_attributes = []}]
                     (Exp.sequence
                        (apply ":=" [ident used_var ; apply "+" [apply "!" [ident used_var] ; Exp.constant (Const.int 1)]])
                        (Exp.sequence
                           (Exp.ifthenelse (apply "=" [apply "mod" [apply "!" [ident used_var] ; int_of_float_of_expr (apply "+." [skip_every ; Exp.constant (Const.float (string_of_float 0.5))])] ; Exp.constant (Const.int 0)])
                              (apply ":=" [ident used_var ; apply "+" [apply "!" [ident used_var] ; Exp.constant (Const.int 1)]]) None)
                           (Exp.let_ Asttypes.Nonrecursive [{ pvb_pat = Pat.var { txt = used_var ; loc = !default_loc } ; pvb_expr = ident "old_i" ; pvb_loc = !default_loc ; pvb_attributes = []}]
                              body))))))
        else
          (* stop early *)
          (Exp.for_ p
             start
             new_absolute_bound
             dir
             body)
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

  List.rev !stdout_lines, List.rev !stderr_lines

let print_both (a, b) =
  print_endline "> stdout" ;
  List.iter print_endline a ;
  print_endline "> stderr" ;
  List.iter print_endline b

let try_perforation ast =
  let results_out = open_out "results.data" in
  Printf.fprintf results_out "# config path time accuracy\n" ;

  let num_loops = List.length !for_loops in

  let run_with_config (config : [`Normal | `Perforated of float list]) =
    let config = match config with `Normal -> replicate 1.0 num_loops | `Perforated ls -> ls in
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

    Printf.printf "> - %s -\n" fout ;

    print_endline "> building..." ;
    print_both @@ (match !build_command with
        | None -> run "ocamlfind" [| "ocamlopt" ; fout ; "-o" ; fout_native |]
        | Some bc ->
          (match Str.split (Str.regexp " ") bc with
           | [] -> failwith ("error: bad command: " ^ bc)
           | command :: args -> run command (Array.of_list (args @ [ fout ; fout_native])))) ;

    print_endline "> running..." ;
    let start_time = Unix.gettimeofday () in
    ignore @@ run fout_native [||] ;
    let total_time = Unix.gettimeofday () -. start_time in
    Printf.printf "> elapsed time: %f sec\n" total_time ;

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

    Printf.fprintf results_out "%s %s %f %f\n" (String.concat "-" (List.map string_of_float used_config)) fout_native total_time fitness ;

    total_time, fitness in

  (* run once with no perforation to get base line *)
  let noperf_time, noperf_fitness = run_with_config `Normal in


  (* run each loop perforated by itself and keep track of its personal scores *)
  let next_perf = ref 0.25 in
  let get_next_perf () =
    let cp = !next_perf in
    if cp > 1.0 then None
    else (next_perf := !next_perf +. 0.25 ; Some cp) in

  let rec iter_until getter f =
    match getter () with
    | None -> ()
    | Some a -> f a ; iter_until getter f in

  let module ScoreMap = Map.Make (struct type t = float let compare = compare end) in

  let loop_scores = Array.init num_loops (fun _ -> ScoreMap.empty ) in
  let update_score i k v =
    loop_scores.(i) <- ScoreMap.add k v loop_scores.(i) in
  let make_solo_conf i p = replicate 1.0 i @ [p] @ replicate 1.0 (num_loops - i - 1) in

  for i = 0 to (num_loops-1) do
    Printf.printf "> perforating loop %d\n" (i+1) ;

    next_perf := 0.25 ;

    iter_until get_next_perf (fun perf ->
        let conf = make_solo_conf i perf in
        let time, fitness = run_with_config (`Perforated conf) in
        let speedup = 1. +. ((noperf_time -. time) /. noperf_time) in
        let accuracy = fitness in
        let score = score speedup accuracy !accuracy_bound in
        Printf.printf "SPEEDUP %f\n" speedup ;
        Printf.printf "ACCURACY %f\n" accuracy ;
        Printf.printf "SCORE %f\n" score ;
        update_score i perf score
      ) ;

  done ;

  Array.iter (fun map ->
      List.iter (fun (k,v) -> Printf.printf "%f - %f\n" k v) (ScoreMap.bindings map)) loop_scores ;

  (* explore *)
  let best = Array.map (fun l -> ScoreMap.bindings l |>
                                 List.fold_left (fun (maxi, maxe) (k, v) ->
                                     let cmp = compare maxe v in
                                     if cmp > 0 || cmp == 0 then
                                       (maxi, maxe)
                                     else
                                       (k, v)) (0., 0.)) loop_scores in

  print_endline "BEST" ;
  Array.iter (fun (k,v) ->
      Printf.printf "%f - %f\n" k v) best ;

  (* start at best configs and hill climb *)

  let time, fitness = run_with_config (`Perforated (List.map fst (Array.to_list best))) in
  let speedup = 1. +. ((noperf_time -. time) /. noperf_time) in
  let accuracy = fitness in

  let rec hill_climb confs best_score step =
    let max_by f z l =
      let rec aux cmax = function
        | [] -> cmax
        | x::xs ->
          let cmp = compare (f x) (f cmax) in
          let new_max = if cmp > 0 then x else cmax in
          aux new_max xs in
      aux z l in
    let rec mixups things solids =
      match things, solids with
      | [], [] -> []
      | t::ts, s::ss ->
        let rest = List.map (fun r -> s :: r) (mixups ts ss)
        in List.map (fun sel -> sel :: ss) t @ rest
      | _ -> assert false in
    let test_config conf =
        let time, fitness = run_with_config (`Perforated conf) in
        let speedup = 1. +. ((noperf_time -. time) /. noperf_time) in
        let accuracy = fitness in
        let score = score speedup accuracy !accuracy_bound in
        Printf.printf "SPEEDUP %f\n" speedup ;
        Printf.printf "ACCURACY %f\n" accuracy ;
        Printf.printf "SCORE %f\n" score ;
        score in
    let gen_test test = mixups (List.map (fun c -> [ c +. step ; c -. step]) test) test in
    let (new_config, new_best_score) =
      (* generate new configs to test *)
      gen_test (Array.to_list confs) |>
      (* test each configuration *)
      List.map (fun c -> c, test_config c) |>
      (* find the best score *)
      max_by snd (Array.to_list confs, best_score) in
    if new_best_score <= best_score then
      (print_endline "no more improvement" ; confs, best_score)
    else
      begin
        let new_step = step -. 0.01 in
        if new_step <= 0. then
          (print_endline "no more steps" ; Array.of_list new_config, new_best_score)
        else
          (Printf.printf "climbing to %s - %f\n" (String.concat " " (List.map string_of_float new_config)) new_best_score ; hill_climb (Array.of_list new_config) new_best_score new_step)
      end in


  let best_config, best_score = hill_climb (Array.map fst best) (score speedup accuracy !accuracy_bound) 0.1 in
  Printf.printf "best improvement : %s - %f" (String.concat " " (List.map string_of_float (Array.to_list best_config))) best_score ;


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
  "--explore", Set auto_explore, "set to auto explore possible perforations" ;
  "--accuracy-bound", Set_float accuracy_bound, "set the accuracy bound in the scoring function" ;
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
