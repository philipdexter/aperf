let main () =
  let fsin = Scanf.Scanning.from_channel stdin in
  let (mn, mpp),(zn, zpp) = Scanf.bscanf fsin "Manhattan norm: %f / per pixel: %f\nZero norm: %f / per pixel: %f\n" (fun a b c d -> (a,b),(c,d)) in
  Scanf.Scanning.close_in fsin ;
  Printf.printf "%f\n" mpp

let () = main ()

(* Manhattan norm: 14169940.7181 / per pixel: 24.0240151607 *)
(* Zero norm: 150519.0 / per pixel: 0.255193074544 *)
