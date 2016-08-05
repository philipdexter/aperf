let for_loops = ref []

let usage_msg =
  Printf.sprintf
    "Usage: %s\n"
    Sys.argv.(0)

let score_function speedup accuracy_loss b =
  if accuracy_loss >= b then 0.
  else 2. /. ( (1. /. (speedup -. 1.)) +. (1. /. (1. -. (accuracy_loss /. b))) )

let calc_speedup old_time new_time = old_time /. new_time

let calc_speedup_accuracy_score old_time time fitness accuracy_loss_bound =
  let speedup = calc_speedup old_time time in
  speedup, fitness, score_function speedup fitness accuracy_loss_bound

let print_stats speedup accuracy score =
  Printf.printf "SPEEDUP %f\n" speedup ;
  Printf.printf "ACCURACY LOSS %f\n" accuracy ;
  Printf.printf "SCORE %f\n" score

let search_exp_mapper mapper e =
  let open Parsetree in
  let open Location in
  let open Lexing in
  let open Ast_mapper in
  let save_it e = for_loops := e :: !for_loops in
  match e with
  | [%expr Aperf.array_iter_approx [%e? f] [%e? l]] ->
    save_it e ;
    default_mapper.expr mapper e
  | [%expr for [%p? _] = [%e? start] to [%e? bound] do [%e? body] done] ->
    for_loops := e :: !for_loops ;
    default_mapper.expr mapper e
  | x -> default_mapper.expr mapper e

let search_mapper =
  let open Parsetree in
  let open Ast_mapper in
  let open Location in
  { default_mapper with
    expr = (fun mapper expr ->
        match expr with
        | [%expr Aperf.array_iter_approx [%e? f] [%e? l]] -> search_exp_mapper mapper expr
        | { pexp_desc = Pexp_extension ({ txt = "perforate" } as loc, PStr [{pstr_desc = Pstr_eval (e,attributes)} as struc])} ->
          { expr with
            pexp_desc = Pexp_extension (loc,
                                        PStr [{ struc with
                                                pstr_desc = Pstr_eval (search_exp_mapper mapper e, attributes) }]) }
        | { pexp_attributes = attr} ->
          if List.exists (fun (a,_) -> try String.sub a.txt 0 9 = "perforate" with Invalid_argument _ -> false) attr then
            search_exp_mapper mapper expr
          else
            default_mapper.expr mapper expr) }

let active_config = ref []

let list_iter_approx = fun _ _ -> failwith "you must use 'aperf' if you want to approximate"
let array_iter_approx = fun _ _ -> failwith "you must use 'aperf' if you want to approximate"

let active_exp_mapper note mapper e =
  let open Parsetree in
  let open Location in
  let open Ast_helper in
  let open Ast_mapper in
  (* TODO add stubs for list.iter_approx *)
  match e with
  | [%expr Aperf.array_iter_approx [%e? f] [%e? l]] ->
    let this_config = List.hd !active_config in
    active_config := List.tl !active_config ;
    let do_perforation = this_config <> 1. in
    if do_perforation then
      begin
        (* don't need ident probably, can use metaquot *)
        let ident i = Exp.ident { txt = Longident.Lident i ; loc = !default_loc } in
        let perforation = this_config in
        let bound = [%expr Array.length [%e l]] in
        let new_relative_bound = [%expr int_of_float (float_of_int [%e bound] *. [%e Exp.constant (Const.float (string_of_float perforation))])] in
        let new_absolute_bound = [%expr [%e new_relative_bound]] in
        let to_note = if fst note then [%expr [%e ident (snd note)] := [%e (Exp.constant (Const.float (string_of_float this_config)))]] else [%expr () ] in
        (* TODO allow to run the computations multiple times *)
        (* TODO make the function array_iter_approx a cousin where it saves the approximation level like array_iter_approx_and_save *)
        (* TODO allow just a @perforatedvar variable
           like let perf = ref 1.0 [@perfvar] *)
        [%expr
          let aperf_break_counter = ref 0 in
          let aperf_end = [%e new_absolute_bound] in
          (* support list iter as well *)
          (try Array.iter (fun a ->
               if !aperf_break_counter >= aperf_end then failwith ""
               else (aperf_break_counter := !aperf_break_counter + 1 ;
                     [%e f] a)) [%e l]
           with _ -> ()) ;
          [%e to_note]
        ]
      end
    else
      default_mapper.expr mapper [%expr Array.iter [%e f] [%e l]]
  | [%expr for [%p? p] = [%e? start] to [%e? bound] do [%e? body] done] ->
    let this_config = List.hd !active_config in
    active_config := List.tl !active_config ;
    let do_perforation = this_config <> 1. in
    if do_perforation then
      begin
        let ident i = Exp.ident { txt = Longident.Lident i ; loc = !default_loc } in
        let perforation = this_config in
        let new_relative_bound = [%expr int_of_float (float_of_int ([%e bound] - [%e start]) *. [%e Exp.constant (Const.float (string_of_float perforation))])] in
        let new_absolute_bound = [%expr [%e start] + [%e new_relative_bound]] in
        (* let skip_every = apply "/." [float_of_int_of_expr bound_minus_start ; apply "-." [float_of_int_of_expr bound_minus_start ; apply "*." [float_of_int_of_expr bound_minus_start ; Exp.constant (Const.float (string_of_float perforation))]]] in *)
        (* temporarily turn off skipping of elements *)
        if false then
          failwith "false"
        (* if perforation > 0.5 then *)
          (* skip elements *)
          (* mapper.expr mapper *)
          (*   (Exp.let_ Asttypes.Nonrecursive [{ pvb_pat = Pat.var { txt = used_var ; loc = !default_loc }  ; pvb_expr = apply "ref" [start] ; pvb_loc = !default_loc ; pvb_attributes = []}] *)
          (*      (Exp.while_ (apply "<" [ apply "!" [ident used_var] ; bound]) *)
          (*         (Exp.let_ Asttypes.Nonrecursive [{ pvb_pat = Pat.var { txt = "old_i" ; loc = !default_loc } ; pvb_expr = apply "!" [ident used_var] ; pvb_loc = !default_loc ; pvb_attributes = []}] *)
          (*            (Exp.sequence *)
          (*               (apply ":=" [ident used_var ; apply "+" [apply "!" [ident used_var] ; Exp.constant (Const.int 1)]]) *)
          (*               (Exp.sequence *)
          (*                  (Exp.ifthenelse (apply "=" [apply "mod" [apply "!" [ident used_var] ; int_of_float_of_expr (apply "+." [skip_every ; Exp.constant (Const.float (string_of_float 0.5))])] ; Exp.constant (Const.int 0)]) *)
          (*                     (apply ":=" [ident used_var ; apply "+" [apply "!" [ident used_var] ; Exp.constant (Const.int 1)]]) None) *)
          (*                  (Exp.let_ Asttypes.Nonrecursive [{ pvb_pat = Pat.var { txt = used_var ; loc = !default_loc } ; pvb_expr = ident "old_i" ; pvb_loc = !default_loc ; pvb_attributes = []}] *)
          (*                     body)))))) *)
        else begin
          (* stop early *)
          let new_for = [%expr
            for [%p p] = [%e start] to [%e new_absolute_bound] do
              [%e body]
            done ] in
          if fst note then
            [%expr [%e new_for] ; [%e ident (snd note)] := [%e (Exp.constant (Const.float (string_of_float this_config)))]]
          else
            new_for
        end
      end
      (* todo make sure the let i = !i -1 is changed to - 2 if necessary , could save old value *)
      (* todo every N skip M -> leads to accuracy *)
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
        (* is this branch needed? maybe add failwith to test if ever fired *)
        | { pexp_desc = Pexp_extension ({ txt = "perforate" }, PStr [{pstr_desc = Pstr_eval (e,attributes)}])} -> active_exp_mapper (false, "") mapper e
        | { pexp_attributes = attr} ->
          if List.exists (fun (a,_) -> try String.sub a.txt 0 9 = "perforate" with Invalid_argument _ -> false) attr then begin
            let arg = match Util.find_and_extract (fun (a, _) -> a.txt = "perforatenote")
                              (fun (_, PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_ident {txt = Longident.Lident exvar}},_)}]) -> exvar) attr with
              | None -> (false, "")
              | Some x -> (true, x) in
            active_exp_mapper arg mapper expr
          end
          else
            default_mapper.expr mapper expr
        | [%expr Aperf.array_iter_approx [%e? f] [%e? p]] -> active_exp_mapper (false, "") mapper expr) }

let run command args =
  let (pr0, pw0) = Unix.pipe () in
  let (pr1, pw1) = Unix.pipe () in
  let (pr2, pw2) = Unix.pipe () in
  let _pid = Unix.create_process command (Array.append [| command |] args) pr0 pw1 pw2 in
  Unix.close pw0 ;
  Unix.close pr0 ;
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

let try_perforation eval_cmd build_cmd explore accuracy_loss_bound results_file ast =
  let results_out = open_out results_file in
  Printf.fprintf results_out "# config path time accuracy\n" ;

  let num_loops = List.length !for_loops in

  let run_with_config (config : [`Normal | `Perforated of float list]) =
    let config = match config with `Normal -> Util.replicate 1.0 num_loops | `Perforated ls -> ls in
    let used_config = config in
    active_config := config ;
    Printf.printf ">>>>\n> running with config:\n> %s\n" (String.concat "-" (List.map string_of_float !active_config)) ;
    let ast' =
      let open Ast_mapper in
      active_mapper.structure active_mapper ast in
    let fout = Filename.temp_file ~temp_dir:"./tmp/" "aperf" ".ml" in
    let fout_native = String.sub fout 0 (String.length fout - 3) ^ ".native" in
    let fn = open_out fout in
    Printf.fprintf fn "%s\n" (Pprintast.string_of_structure ast') ;
    close_out fn ;

    Printf.printf "> - %s -\n" fout ;

    print_endline "> building..." ;
    print_both
      (match Str.split (Str.regexp " ") build_cmd with
       | [] -> failwith ("error: bad command: " ^ build_cmd)
       | command :: args -> run command (Array.of_list (args @ [ fout ; fout_native]))) ;

    print_endline "> running..." ;
    let start_time = Unix.gettimeofday () in
    ignore @@ run fout_native [||] ;
    let total_time = Unix.gettimeofday () -. start_time in
    Printf.printf "> elapsed time: %f sec\n" total_time ;

    print_endline "> evaluating..." ;
    let fitness =
      let fitness =
        let stdout, stderr = run eval_cmd [| fout_native |] in
        match stdout with
        | [fs] -> (try (abs_float (float_of_string fs)) with _ -> failwith (String.concat "" stdout))
        | _ -> failwith (String.concat "" stdout) in
      Printf.printf "> fitness: %f\n" fitness ;
      fitness in

    Printf.fprintf results_out "%s %s %f %f\n" (String.concat "-" (List.map string_of_float used_config)) fout_native total_time fitness ;

    total_time, fitness in

  (* run once with no perforation to get base line *)
  print_endline "> running baseline..." ;
  let noperf_time, noperf_fitness = run_with_config `Normal in

  (* helper function to calculate stats based on baseline *)
  let calc_stats = calc_speedup_accuracy_score noperf_time in

  (* we never want to run a configuration below 0 or above 1 *)
  let clamp_all = List.map (Util.clamp 0.0 1.0) in

  (* function to test each configuration with *)
  let test_config conf =
    let time, fitness = run_with_config (`Perforated (clamp_all conf)) in
    let speedup, accuracy_loss, score = calc_stats time fitness accuracy_loss_bound in
    print_stats speedup accuracy_loss score ;
    { Config_run.conf
    ; Config_run.time
    ; Config_run.speedup
    ; Config_run.accuracy_loss
    ; Config_run.score } in

  (*
   * choose which config runner to use
   * right now it's either exhaustive search or hill climbing
   *)
  let runner = if explore then (module Config_run.HillClimb : Config_run.T) else (module Config_run.Exhaustive) in

  let best_config, best_config_result =
    let (module Runner) = runner in
    Runner.run num_loops test_config |>
    Util.max_by_1 (fun (_,c) -> c.Config_run.score) in

  Printf.printf "best improvement : %s - %f"
    (String.concat " " (List.map string_of_float best_config))
    best_config_result.Config_run.score ;

  close_out results_out


let aperf eval build explore accuracy_loss_bound results_file perf_file =
  print_endline "input file:" ;
  print_endline perf_file ;

  Location.input_name := perf_file ;
  let fmt = Format.std_formatter in
  let lexer = Lexing.from_channel (open_in perf_file) in
  let pstr = Parse.implementation lexer in

  let pstr' = search_mapper.Ast_mapper.structure search_mapper pstr in

  Printf.printf "> found %d for loops for perforation\n" (List.length !for_loops) ;

  List.iter (fun e -> Format.pp_print_string fmt ">>\n" ; Pprintast.expression fmt e ; Format.pp_print_newline fmt ()) !for_loops ;

  try_perforation eval build explore accuracy_loss_bound results_file pstr'


open Cmdliner

let aperf =
  let version = "%%VERSION%%" in

  (* options *)
  (* TODO add accuracy bound argument *)
  (* TODO expose tmp directory to use *)
  let opt_eval =
    let doc = "Run CMD to evaluate the accuracy of each result" in
    let docv = "CMD" in
    Arg.(required & opt (some string) None & info ["E" ; "eval"] ~doc ~docv) in
  let opt_build =
    let doc = "Run CMD to build each configuration" in
    let docv = "CMD" in
    (* TODO make this only required if --explore is set *)
    Arg.(required & opt (some string) None & info ["B" ; "build"] ~doc ~docv) in
  let opt_results_file =
    let doc = "Save results to FILE (defaults to results.data)" in
    let docv = "FILE" in
    Arg.(value & opt string "results.data" & info ["o" ; "results-file"] ~doc ~docv) in
  let opt_explore =
    let doc = "Explore the search space using a fitness function" in
    Arg.(value & flag & info ["e" ; "explore"] ~doc) in
  let opt_accuracy_loss_bound =
    let doc = "Accept accuracy losses up to FLOAT (defaults to 0.30)" in
    let docv = "FLOAT" in
    Arg.(value & opt float 0.30 & info ["A" ; "accuracy_loss_bound"] ~doc ~docv) in
  let file =
    let doc = "Annotated OCaml source file" in
    let docv = "FILE" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv) in
  let term = Term.(const aperf $ opt_eval $ opt_build $ opt_explore $ opt_accuracy_loss_bound $ opt_results_file $ file) in

  (* help page *)
  let doc = "Perforation tools" in
  let man =
    [ `S "DESCRIPTION" ;
      `P "$(b,$(mname)) tries to perforate loops in OCaml programs" ;
      `S "AUTHOR" ;
      `P "Philip Dexter, $(i,http://phfilip.com)" ;
      `S "REPORTING BUGS" ;
      `P "Report bugs on the GitHub project page %%PKG_HOMEPAGE%%" ;
    ] in
  let info = Term.info "aperf" ~version ~man ~doc in

  (term, info)

let () = match Term.eval aperf with
  | `Error _ -> exit 1
  | _ -> exit 0
