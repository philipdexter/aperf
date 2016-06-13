open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

exception Undefined of string;;

let perforate_property attributes =
  let rec loop attrs =
    match attrs with
    | [] -> attrs, None
    | (loc,payload) :: attrs' -> if String.equal "perforate" loc.txt
                                 then
                                   let i =
                                     match payload with
                                       PStr [{pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_integer (i,_)) }, _)}] -> int_of_string i in
                                   (attrs', Some i)
                                 else
                                   match loop attrs' with
                                     (attrs'', r) -> ((loc,payload) :: attrs'', r) in
  loop attributes

let perf_expression mapper exp attributes =
  match exp.pexp_desc with
  | Pexp_for (({ ppat_desc = Ppat_var p_var} as p), start, bound, dir, body) ->
     let old_start =
       match start with
       | { pexp_desc = Pexp_constant (Pconst_integer (i,_)) } -> int_of_string i in
     let old_bound =
       match bound with
       | { pexp_desc = Pexp_constant (Pconst_integer (i,_)) } -> int_of_string i in
     let (pexp_attributes, perforation) =
       match perforate_property attributes with
       | (attributes, sn) -> (attributes, match sn with Some n -> n | None -> (old_bound - old_start) / 2) in
     let new_exp = [%expr
                       for i = 0 to [%e { bound with pexp_desc = Pexp_constant (Pconst_integer (string_of_int perforation, None)) } ] do [%e body] done
                   ] in
     mapper.expr mapper { new_exp with pexp_attributes }


let perforate_mapper argv =
  { default_mapper with
    expr = (fun mapper expr ->
           match expr with
           | [%expr [%perforate [%e? exp]]] -> perf_expression mapper exp expr.pexp_attributes
           | { pexp_attributes = attr} -> (match perforate_property attr with
                                           | (_, Some p) -> perf_expression mapper expr attr
                                           | (_, None) -> default_mapper.expr mapper expr)
           | x -> default_mapper.expr mapper x) ;
  }

let () = register "perforate" perforate_mapper
