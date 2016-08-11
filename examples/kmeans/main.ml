let read_point line =
  let open Point in
  match List.map float_of_string (Str.split (Str.regexp ",") line) with
  | [x; y] -> { x ; y }
  | _ -> assert false

let read_points path =
  let fin = open_in path in
  let points = ref [] in
  let points = (try
                  while true do
                    points := read_point (input_line fin) :: !points
                  done ; !points
                with End_of_file ->
                  (close_in fin ; List.rev !points)) in
  points

let out_centroids fout centroids =
  List.iter (fun {Point.x=x;Point.y=y} -> Printf.fprintf fout "%f %f\n" x y) @@ List.sort (fun {Point.x=x} {Point.x=x'} -> compare x x') centroids

let () =
  let fout = open_out "kmeans.out" in
  for i = 1 to 30 do
    let file = "inputs/"^(string_of_int i)^".in" in
    let points = read_points file in
    let centroids, _ = Kmeans.run points 15 10 in
    out_centroids fout centroids ;
  done ;
  close_out fout
