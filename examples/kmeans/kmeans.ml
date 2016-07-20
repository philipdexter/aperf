open Point

let min_by f xs =
  let h = List.hd xs in
  let m = f h in
  let (_, min_elt) = List.fold_left (fun (a, x) y -> let b = f y in (if b < a then (b, y) else (a, x))) (m, h) xs in
  min_elt

let closest p qs = min_by (dist p) qs

let sum qs = List.fold_left ( ++ ) {x = 0.0; y = 0.0} qs

let average qs = (sum qs) // (qs |> List.length |> float_of_int)

module PTable = Hashtbl.Make(struct type t = Point.t let hash = Hashtbl.hash let equal = fun x y -> compare x y = 0 end)

let group_by xs f =
  let table = PTable.create 100000 in
  let nl = Array.of_list xs in
  for i = 0 to Array.length nl - 1 do
    let x = nl.(i) in
    let y = f x in
    let to_add = try PTable.find table y with Not_found -> [] in
    PTable.replace table y (x :: to_add)
  done [@perforate] ;
  table

let clusters xs centroids =
  group_by xs (fun p -> closest p centroids)
  |> fun tbl -> PTable.fold (fun _ b acc -> b :: acc) tbl []

let rec take xs = function
  | 0 -> []
  | n -> (match xs with
      | [] -> []
      | x::xs' -> x :: take xs' (n-1))

let run points iters k =
  let centroids = ref (take points k) in
  let new_clusters = ref (clusters points !centroids) in
  for i = 1 to iters do
    centroids := List.map average !new_clusters ;
    new_clusters := clusters points !centroids;
  done [@perforate] ;
  !centroids, !new_clusters
