let id x = x
let ( *** ) f g = fun (x,y) -> (f x, g y)
let second f = id *** f
let first f = f *** id
let both f = f *** f

let configs = ref false

let turn_on_configs () = configs := true

let _ =
  Arg.parse
    (let open Arg in
     align [
       "--configs", Unit turn_on_configs, "Turn on config labelling" ;
     ]) (fun _ -> ()) "..." ;

  let fin = open_in "results.data" in

  let data_points = ref [] in

  (try
     while true do
       data_points := Str.split (Str.regexp " ") (input_line fin) :: !data_points
     done
   with End_of_file -> ()) ;

  let labels, (xs, ys) =
    !data_points |>
    List.rev |>
    List.tl |>
    List.map (fun [a;b;c;d] -> a,(float_of_string c,float_of_string d)) |>
    List.split |>
    second List.split |>
    second (both Array.of_list)
  in

  let open Plplot in
  let module P = Plot in

  let max_x = Array.fold_left max 0. xs in
  let max_y = Array.fold_left max 0. ys in

  let p = P.init (0., 0.) (both ceil (max_x, max_y)) `greedy (`window `cairo) in

  if !configs then
    P.plot ~stream:p @@
    List.mapi (fun i s ->
        P.text `black xs.(i) (ys.(i)+.0.05) s) labels ;

  P.plot ~stream:p [P.points `blue xs ys] ;

  P.finish ~stream:p ()
