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
  (* To increase the amount of work we sum the numbers multiple times.*)
  for i = 1 to 100000 do
    answer := 0;
    let perforated = ref 1. in
    Aperf.array_iter_approx (fun a -> answer := !answer + a) arr [@perforatenote perforated] ;
    answer := int_of_float ((float_of_int !answer) *. (1. /. !perforated)) ;
  done ;
  !answer

let main () =
  let out = open_out "sum.out" in
  for i = 1 to 30 do
    let filename = Printf.sprintf "input.%d" i in
    let f = open_in filename in
    let sum = sum_from f in
    Printf.fprintf out "%d\n" sum ;
    close_in f
  done ;
  close_out out

let () = main ()
