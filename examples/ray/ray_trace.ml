(* based on a ray tracer found at http://www.ffconsultancy.com/ocaml/ray_tracer/index.html *)

(* Generic maths *)
let delta = sqrt epsilon_float and pi = 4. *. atan 1.
let sqr x = x *. x
let sin t = sin(pi *. t /. 180.) and cos t = cos(pi *. t /. 180.)
let clamp l u x = if x < l then l else if x > u then u else x
type vec = float array
let vec3 x y z = [| x; y; z; 1. |]
let zero = vec3 0. 0. 0.
let ( *| ) s r = [| s *. r.(0); s *. r.(1); s *. r.(2); 1. |]
let ( +| ) a b = [| a.(0) +. b.(0); a.(1) +. b.(1); a.(2) +. b.(2); 1. |]
let ( -| ) a b = [| a.(0) -. b.(0); a.(1) -. b.(1); a.(2) -. b.(2); 1. |]
let dot a b = a.(0) *. b.(0) +. a.(1) *. b.(1) +. a.(2) *. b.(2)
let length2 r = dot r r
let length r = sqrt(length2 r)
let unitise r = (1. /. length r) *| r
let init f = Array.init 4 (fun i -> Array.init 4 (f i))
let translate r =
  init (fun i j -> if i = j then 1. else if j = 3 then r.(i) else 0.)
let identity = translate (vec3 0. 0. 0.)
let sum4 f = f 0 +. f 1 +. f 2 +. f 3
let mat_mul a b = init (fun i j -> sum4 (fun k -> a.(i).(k) *. b.(k).(j)))
let transform m r = Array.init 4 (fun i -> dot m.(i) r +. m.(i).(3))
let rot_x t = let cos, sin = cos t, sin t in [|[|    1.;    0.;    0.; 0. |];
                                               [|    0.;   cos;   sin; 0. |];
                                               [|    0.; -.sin;   cos; 0. |];
                                               [|    0.;    0.;    0.; 1. |]|]
let rot_y t = let cos, sin = cos t, sin t in [|[|   cos;    0.;   sin; 0. |];
                                               [|    0.;    1.;    0.; 0. |];
                                               [| -.sin;    0.;   cos; 0. |];
                                               [|    0.;    0.;    0.; 1. |]|]
let rot_z t = let cos, sin = cos t, sin t in [|[|   cos;   sin;    0.; 0. |];
                                               [| -.sin;   cos;    0.; 0. |];
                                               [|    0.;    0.;    1.; 0. |];
                                               [|    0.;    0.;    0.; 1. |]|]

(* Ray tracing primitives *)
type material = { color: float * float * float; shininess: float }
type sphere = { center: vec; radius: float }
type obj = Sphere of sphere * material | Group of sphere * obj list
type ray = { origin: vec; direction: vec }

(* Ray-sphere intersection *)
let ray_sphere ray sphere =
  let v = sphere.center -| ray.origin in
  let b = dot v ray.direction in
  let disc = sqr b -. length2 v +. sqr sphere.radius in
  if disc < 0. then infinity else
    let disc = sqrt disc in
    let t2 = b +. disc in
    if t2 < 0. then infinity else
      let t1 = b -. disc in
      if t1 > 0. then t1 else t2

(* Find the first intersection of the given ray with the given set of
   objects. *)
let intersect ray scene =
  let rec of_scene ((l, _, _) as first) = function
      Sphere (sphere, material) ->
        let l' = ray_sphere ray sphere in
        if l' >= l then first else (* No nearer *)
          let normal () =
            unitise (ray.origin +| l' *| ray.direction -| sphere.center) in
          l', normal, material (* Replace with nearer intersection *)
    | Group (bound, scenes) ->
        let l' = ray_sphere ray bound in (* Cull if possible *)
        if l' >= l then first else List.fold_left of_scene first scenes in
  let first =
    if ray.direction.(1) < 0. then
      (* Floor *)
      let l = -. ray.origin.(1) /. ray.direction.(1) in
      let r = ray.origin +| l *| ray.direction in
      let x = length [|r.(0); 0.; r.(2); 1.|] in
      let g = 0.5 *. (1. +. cos(180. *. x)) *. exp(-. 0.05 *. x *. x) in
      l, (fun () -> vec3 0. 1. 0.), { color = g, g, g; shininess = 0. }
    else
      infinity, (fun () -> zero), { color = 0., 0.1, 0.2; shininess = 0. } in
  of_scene first scene

(* Trace a single ray *)
let rec ray_trace weight light ray scene = match intersect ray scene with
    lambda, normal, material ->
      if lambda = infinity then 0., 0., 0. else
        let n = normal () in
        let o = ray.origin +| lambda *| ray.direction +| delta *| n in
        (* Recursively examine specular reflections *)
        let r, g, b =
          if weight < 0.5 then 0., 0., 0. else
            let d = ray.direction in
            let ray = { origin = o; direction = d -| (2. *. dot d n) *| n } in
            let s = material.shininess in
            match ray_trace (weight *. s) light ray scene with
              r, g, b -> s *. r, s *. g, s *. b in
        (* Calculate the final color, taking account of shadows, specular and
           diffuse reflection. *)
        let f = max 0. (-. dot n light) in
        let s =
          match intersect { origin = o; direction = zero -| light } scene with
            l, _, _ when l = infinity -> f
          | _ -> 0. in
        match material.color with
          r', g', b' -> (s +. r) *. r', (s +. g) *. g', (s +. b) *. b'

exception Finished
let () =
  (* Get the level of detail from the command line *)
  let level = 6 in

  (* Initialise OpenGL and glut *)
  let width = ref 768 and height = ref 768 in

  (* Define the scene *)
  let light = unitise (vec3 (-1.) (-3.) 2.) in
  let s = 1. /. 3. in (* Radius ratio of one sphere to the next *)
  let count = ref 0 in
  let scene =
    let rec aux level r m =
      incr count;
      let sphere = { center = transform m zero; radius = r } in
      let material =
        let t = 15. *. float level in
        { color = sin t, sin(t +. 60.), sin(t +. 120.); shininess = 0.5 } in
      let obj = Sphere (sphere, material) in
      if level = 1 then obj else begin
        let objects = ref [] in

        for i = 0 to 2 do
          let m = mat_mul m (rot_y (30. +. 120. *. float i)) in
          let m = mat_mul m (rot_z 45.) in
          let m = mat_mul m (translate (vec3 0. ((1. +. s) *. r) 0.)) in
          objects := aux (level - 1) (s *. r) m :: !objects
        done;

        for i = 0 to 5 do
          let m = mat_mul m (rot_y (60. *. float i)) in
          let m = mat_mul m (rot_z 110.) in
          let m = mat_mul m (translate (vec3 0. ((1. +. s) *. r) 0.)) in
          objects := aux (level - 1) (s *. r) m :: !objects
        done;

        (* Bounding radius *)
        let r' =
          List.fold_left
            (fun r -> function Sphere (sph', _) | Group (sph', _) ->
               max r (length (sph'.center -| sphere.center) +. sph'.radius))
            r !objects in

        Group ({ sphere with radius = r' }, obj :: !objects)
      end in
    aux level 1. (translate (vec3 0. 1. 0.)) in
  Printf.printf "Created %d objects\n" !count;

  let pixels = ref [] in
  let started = ref 0. in


  (* Render incrementally when idle *)
  let idle () =
    for y = 0 to !height - 1 do
      for x = 0 to !width - 1 do
        let ray =
          let w, h = float !width, float !height in
          let o = vec3 0. 2. (-6.) in
          let x, y = float x -. 0.5 *. w, float y -. 0.5 *. h in
          let d = unitise (vec3 x y (max w h)) in
          let d = transform (rot_x (-9.)) d in
          { origin = o ; direction = d } in

        pixels := (x, y, ray_trace 1. light ray scene) :: !pixels
      done [@perforate]
    done [@perforate] ;
    Printf.printf "Took: %fs\n" (Sys.time () -. !started) in

  let open Images in


  (* Display the current render *)
  let display () =
    let img = Rgb24.create !width !height in
    let aux (x, y, (r, g, b)) =
      let f x = clamp 0 255 (int_of_float (255. *. x)) in
      Rgb24.set img (!width-x-1) (!height-y-1) {r = f r ; g = f g ; b = f b } in
      (* GlDraw.color (f r, f g, f b); *)
      (* GlDraw.vertex2 (float x, float y) in *)
    List.iter aux !pixels;
    let tiff_name = Sys.argv.(0) ^ ".tiff" in
    Tiff.save tiff_name [] (Images.Rgb24 img) in

  let start_time = Unix.gettimeofday () in
  idle () ;

  let end_time = Unix.gettimeofday () in

  Printf.eprintf "-- took %f\n" (end_time -. start_time) ;

  let start_time = Unix.gettimeofday () in

  display () ;

  let end_time = Unix.gettimeofday () in

  Printf.eprintf "-- took %f\n" (end_time -. start_time)
