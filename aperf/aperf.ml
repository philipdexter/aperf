let repeat_runs = 10

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

let list_iter_approx = fun _ _ -> failwith "you must use 'aperf' if you want to approximate"
let array_iter_approx = fun _ _ -> failwith "you must use 'aperf' if you want to approximate"

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

let try_perforation eval_cmd build_cmd explore accuracy_loss_bound results_file ast pps =
  let results_out = open_out results_file in
  Printf.fprintf results_out "# config path time accuracy\n" ;

  let num_loops = List.length pps in

  let run_with_config (config : [`Normal | `Perforated of float list]) =
    let config = match config with `Normal -> Util.replicate 1.0 num_loops | `Perforated ls -> ls in
    let used_config = config in
    Printf.printf ">>>>\n> running with config:\n> %s\n" (String.concat "-" (List.map string_of_float config)) ;
    let ast' = Perforate.perforate ast config in
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

    print_endline "> running 10 times..." ;

    let run_once () =
      let start_time = Unix.gettimeofday () in
      ignore @@ run fout_native [||] ;
      let total_time = Unix.gettimeofday () -. start_time in
      Printf.printf "> elapsed time: %f sec\n" total_time ;
      total_time in

    (* run the program repeat_runs (10) times to get an average run time *)
    let all_times = List.map run_once (Util.replicate () repeat_runs) in
    let total_time =
      let len = repeat_runs in
      let sum = List.fold_left (+.) 0. all_times in
      sum /. (float_of_int len) in


    (* we assume that the run will always yield the same fitness, therefore we only
       evaluate it once *)
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

  Printf.printf "best improvement : config %s - speedup %f - accuracy loss %f - score %f"
    (String.concat " " (List.map string_of_float best_config))
    best_config_result.Config_run.speedup
    best_config_result.Config_run.accuracy_loss
    best_config_result.Config_run.score ;

  close_out results_out


let aperf eval build explore accuracy_loss_bound results_file perf_file =
  print_endline "input file:" ;
  print_endline perf_file ;

  Location.input_name := perf_file ;
  let fmt = Format.std_formatter in
  let lexer = Lexing.from_channel (open_in perf_file) in
  let pstr = Parse.implementation lexer in

  let (pps, pstr') = Perforate.find pstr in

  Printf.printf "> found %d for loops for perforation\n" (List.length pps) ;

  List.iter (fun e -> Format.pp_print_string fmt ">>\n" ; Pprintast.expression fmt e ; Format.pp_print_newline fmt ()) pps ;

  try_perforation eval build explore accuracy_loss_bound results_file pstr' pps


open Cmdliner

let aperf =
  let version = "0.1.2" in

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
