type config = float list
type config_result =
  { conf : config
  ; time : float
  ; speedup : float
  ; accuracy_loss : float
  ; score : float
  }

module type T = sig
  (** num_loops -> config_run_function -> list of configs and their results *)
  val run : int -> (config -> config_result) -> (config * config_result) list
end

module Exhaustive : T = struct
  let run num_loops f =
    let perfs = Util.perforations [0.0; 0.05; 0.1; 0.15; 0.2; 0.25; 0.3; 0.35; 0.4; 0.45; 0.5; 0.55; 0.6; 0.65; 0.7; 0.75; 0.8; 0.85; 0.9; 0.95] num_loops in
    List.map (fun c -> c, f c) perfs
end


module HillClimb : T = struct
  let run num_loops f =
    (* run each loop perforated by itself and keep track of its personal scores *)

    let module ScoreMap = Map.Make (struct type t = float let compare = compare end) in

    let loop_scores = Array.init num_loops (fun _ -> ScoreMap.empty ) in
    let update_score i k v =
      loop_scores.(i) <- ScoreMap.add k v loop_scores.(i) in
    let make_solo_conf i p = Util.replicate 1.0 i @ [p] @ Util.replicate 1.0 (num_loops - i - 1) in

    let next_perf = ref 0. in
    let next_config () =
      if !next_perf >= 1. then None
      else (let cp = !next_perf in
            next_perf := !next_perf +. 0.25;
            Some cp) in

    for i = 0 to (num_loops-1) do
      Printf.printf "> perforating loop %d\n" (i+1) ;

      next_perf := 0. ;

      Util.iter_until next_config (fun perf ->
          let conf = make_solo_conf i perf in
          let {score=score} = f conf in
          update_score i perf score
        ) ;

    done ;

    Array.iter (fun map ->
        List.iter (fun (k,v) -> Printf.printf "%f - %f\n" k v) (ScoreMap.bindings map)) loop_scores ;

    (* best is a list of form (config, score) where the ith element is the best perforation score for loop i *)
    let best = Array.map (fun l -> ScoreMap.bindings l |>
                                   Util.max_by snd (0.,0.)) loop_scores in

    print_endline "best configurations" ;
    Array.iter (fun (k,v) ->
        Printf.printf "%f - %f\n" k v) best ;


    (* start at best configs and hill climb *)
    let best_config_result = f (List.map fst (Array.to_list best)) in

    let rec hill_climb confs best_config_result step =
      let gen_test test = Util.mixups (List.map (fun c -> [ c +. step ; c -. step]) test) test in
      let new_config, new_best_config_result =
        (* generate new configs to test *)
        gen_test confs |>
        (* test each configuration *)
        List.map (fun c -> c, f c) |>
        (* find the best score *)
        Util.max_by_1 (fun (_,c) -> c.score) in
      if new_best_config_result.score <= best_config_result.score then
        (print_endline "no more improvement" ; confs, best_config_result)
      else
        begin
          let new_step = step -. 0.01 in
          if new_step <= 0. then
            (print_endline "no more steps" ; new_config, new_best_config_result)
          else
            (Printf.printf "climbing to %s - %f\n" (String.concat " " (List.map string_of_float new_config)) new_best_config_result.score ; hill_climb new_config new_best_config_result new_step)
        end in

    [hill_climb (Array.to_list (Array.map fst best)) best_config_result 0.1]
end
