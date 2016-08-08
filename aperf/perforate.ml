(* TODO abstract the searcher and the do-er *)

type perf_points = perf_point list
and perf_point = Parsetree.expression

let for_loops = ref []

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



let find str =
  let str' = search_mapper.Ast_mapper.structure search_mapper str
  in (!for_loops, str')

let active_config = ref []

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


let perforate str config =
  active_config := config ;
  let open Ast_mapper in
  active_mapper.structure active_mapper str
