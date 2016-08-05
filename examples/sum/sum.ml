(** We write this code so stupidly because we need a for-loop to
    perforate.

    If aperf allowed perforation over input streams then we could sum
    the numbers directly instead of first storing them into an array
    and then looping.

    To increase the amount of work we sum the numbers multiple times.
*)

(* let sum_all ls = *)
(*   let agg = ref 0 in *)
(*   let perforated = ref 1. in *)
(*   for i = 0 to Array.length ls - 1 do *)
(*     agg := ls.(i) + !agg *)
(*   done [@perforatenote perforated] ; *)
(*   agg := int_of_float ((float_of_int !agg) *. (1. /. !perforated)) ; *)
(*   !agg *)


let sum_from input =
  let nums =
    let nums = ref [] in
    try
      while true do
        nums := int_of_string (input_line input) :: !nums
      done ; !nums
    with End_of_file -> !nums in
  let answer = ref 0 in

  let arr = Array.of_list nums in
  for i = 1 to 500 do
    answer := 0;
    let perforated = ref 1. in
    Aperf.array_iter_approx (fun a -> answer := !answer + a) arr [@perforatenote perforated] ;
    answer := int_of_float ((float_of_int !answer) *. (1. /. !perforated)) ;
    (* answer := sum_all arr *)
  done ;
  !answer

let main () =
  let out = open_out "sum.out" in
  for i = 1 to 10 do
    let filename = Printf.sprintf "input.%d" i in
    let f = open_in filename in
    let sum = sum_from f in
    Printf.fprintf out "%d\n" sum ;
    close_in f
  done ;
  close_out out

let () = main ()
