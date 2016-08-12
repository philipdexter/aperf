let rec perforations list = function
  | 0 -> []
  | 1 -> List.map (fun x -> [x]) list
  | len ->
    let rest = perforations list (len-1) in
    List.concat @@ List.map (fun xs -> List.map (fun x -> x :: xs) list) rest

let rec replicate e = function
  | 0 -> []
  | n -> e :: replicate e (n-1)

let clamp l u a = if a < l then l else if a > u then u else a

let max_by f z l =
  let rec aux cmax = function
    | [] -> cmax
    | x::xs ->
      let cmp = compare (f x) (f cmax) in
      let new_max = if cmp > 0 then x else cmax in
      aux new_max xs in
  aux z l

let max_by_1 f l =
  match l with
  | [] -> failwith "max_by_1: empty list"
  | (x::xs) -> max_by f x xs

let rec map_over_iter getter f =
  match getter () with
  | None -> []
  | Some a -> f a :: map_over_iter getter f

let rec iter_until getter f =
  match getter () with
  | None -> ()
  | Some a -> f a ; iter_until getter f

let rec mixups things solids =
  match things, solids with
  | [], [] -> []
  | t::ts, s::ss ->
    let rest = List.map (fun r -> s :: r) (mixups ts ss)
    in List.map (fun sel -> sel :: ss) t @ rest
  | _ -> failwith "mixups, lists must be of equal length"

let rec find_and_extract f e = function
  | [] -> None
  | x :: xs -> if f x then Some (e x) else find_and_extract f e xs

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
