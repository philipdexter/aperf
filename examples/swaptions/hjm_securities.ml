let num_trials = ref 102400

let nSwaptions = ref 1

let iN = 11

let iFactors = 3

type parm =
  { id : int64
  ; mutable dSimSwaptionMeanPrice : float
  ; mutable dSimSwaptionStdError : float
  ; dStrike : float
  ; dCompounding : float
  ; dMaturity : float
  ; dTenor : float
  ; dPaymentInterval : float
  ; iN : int
  ; dYears : float
  ; iFactors : int
  ; pdYield : float array
  ; ppdFactors : float array array
  }


let iFactors = 3
let swaptions = ref [||]

let seed = ref (Int64.of_int 1979)

let swaption_seed = ref Int64.zero

let dmatrix nrl nrh ncl nch =
  let nrow = nrh-nrl+1 in
  let ncol = nch-ncl+1 in
  let m = Array.init nrow (fun _ -> Array.make ncol 0.0) in
  m

let dmax da db = if da > db then da else db

let dvector nl nh =
  Array.make (nh-nl+1) 0.0


let hjm_Yield_to_Forward
    pdForward (* Forward curve to be outputted *)
    iN        (* Number of time-steps *)
    pdYield   (* Input yield curve *) =
  (* This function computes forward rates from supplied yield rates. *)

  (* forward curve computation *)
  pdForward.(0) <- pdYield.(0) ;
  for i = 1 to (iN-1) do
    (* as per formula *)
    pdForward.(i) <- (float_of_int (i+1))*.pdYield.(i) -. (float_of_int i)*.pdYield.(i-1)
  done

let hjm_Drifts
    pdTotalDrift (* Output vector that stores the total drift correction for each maturity *)
    ppdDrifts (* Output matrix that stores drift correction for each factor for each maturity *)
    iN
    iFactors
    dYears
    ppdFactors (* Input factor volatilities *) =
  (* This function computes drift corrections required for each
     factor for each maturity based on given factor volatilities *)

  let ddelt = dYears/.(float_of_int iN) in
  let dSumVol = ref 0. in

  (* computation of factor drifts for shortest maturity *)
  for i = 0 to (iFactors-1) do
    ppdDrifts.(i).(0) <- 0.5*.ddelt*.(ppdFactors.(i).(0))*.ppdFactors.(i).(0)
  done ;

  (* computation of factor drifts for other maturities *)
  for i = 0 to (iFactors-1) do
    for j = 1 to (iN-2) do
      ppdDrifts.(i).(j) <- 0. ;
      for l = 0 to (j-1) do
        ppdDrifts.(i).(j) <- ppdDrifts.(i).(j) -. ppdDrifts.(i).(l)
      done ;
      dSumVol := 0. ;
      for l = 0 to j do
        dSumVol := !dSumVol +. ppdFactors.(i).(l)
      done ;
      ppdDrifts.(i).(j) <- ppdDrifts.(i).(j) +. 0.5*.ddelt*. !dSumVol*. !dSumVol
    done
  done ;

  (* computation of total drifts for all maturities *)
  for i = 0 to (iN-2) do
    pdTotalDrift.(i) <- 0. ;
    for j = 0 to (iFactors-1) do
      pdTotalDrift.(i) <- pdTotalDrift.(i) +. ppdDrifts.(j).(i)
    done
  done

let ranUnif s =
  (* uniform random number generator *)
  let open Int64 in
  let k1 = div !s 127773L in
  s := sub (mul 16807L (sub !s (mul k1 127773L))) (mul k1 2836L) ;
  if (!s < 0L) then s := (add !s 2147483647L) ;
  (to_float !s) *. 4.656612875e-10


let a = [|
  2.50662823884 ;
  -18.61500062529 ;
  41.39119773534 ;
  -25.4410604963 ;
|]

let b = [|
  -8.47351093090 ;
  23.08336743743 ;
  -21.06224101826 ;
  3.13082909833
|]

let c = [|
  0.3374754822726147 ;
  0.9761690190917186 ;
  0.1607979714918209 ;
  0.0276438810333863 ;
  0.0038405729373609 ;
  0.0003951896511919 ;
  0.0000321767881768 ;
  0.0000002888167364 ;
  0.0000003960315187
|]

let cumNormalInv u =

  (* Returns the inverse of cumulative normal distribution function. *)
  (* Reference: Moro, B., 1995, "The Full Monte," RISK (February), 57-58. *)
  let x = u -. 0.5 in

  let r = ref 0. in

  if abs_float x < 0.42 then
    begin
    r := x *. x ;
    r := x *. ((( a.(3)*. !r +. a.(2)) *. !r +. a.(1)) *. !r +. a.(0))/.
          ((((b.(3) *. !r+. b.(2)) *. !r +. b.(1)) *. !r +. b.(0)) *. !r +. 1.0) ;
    !r
    end
  else
    begin
      r := u ;
      if x > 0.0 then r := 1.0 -. u ;
      r := log (-.(log !r)) ;
      r := c.(0) +. !r *. (c.(1) +. !r *.
                        (c.(2) +. !r *. (c.(3) +. !r *.
                          (c.(4) +. !r *. (c.(5) +. !r *. (c.(6) +. !r *. (c.(7) +. !r*.c.(8))))))));
      if x < 0.0 then r := -. !r ;
        !r
    end

let discount_Factors_Blocking
    iN
    dYears
    pdRatePath =

  let pdDiscountFactors = Array.make iN 1.0 in

  let ddelt = dYears/.(float_of_int iN) in

  let pdexpRes = Array.init (iN-1) (fun i -> exp (-.pdRatePath.(i)*.ddelt)) in

  for i = 1 to (iN-1) do
    for j = 0 to i-1 do
      pdDiscountFactors.(i) <- pdDiscountFactors.(i) *. pdexpRes.(j)
    done
  done ;

  pdDiscountFactors

let hjm_SimPath_Forward_Blocking
     iN (* Number of time-steps *)
     iFactors (* Number of factors in the HJM framework *)
     dYears (* Number of years *)
     pdForward (* t=0 Forward curve *)
     pdTotalDrift (* Vector containing total drift corrections for different maturities *)
     ppdFactors (* Factor volatilities *)
     lRndSeed (* Random number seed *) =
  (* //This function computes and stores an HJM Path for given inputs *)

  let pdz = dmatrix 0 (iFactors-1) 0 (iN - 1) in
  let randz = dmatrix 0 (iFactors-1) 0 (iN - 1) in

  let dTotalShock = ref 0. in

  let ddelt = dYears/.(float_of_int iN) in
  let sqrt_ddelt = sqrt(ddelt) in

  let ppdHJMPath = dmatrix 0 (iN-1) 0 (iN-1) in

  (* ===================================================== *)
  (* t=0 forward curve stored iN first row of ppdHJMPath *)
  (* At time step 0: insert expected drift *)
  (* rest reset to 0 *)
  for j = 0 to (iN-1) do
    ppdHJMPath.(0).(j) <- pdForward.(j)
  done ;
  (* ----------------------------------------------------- *)

  (* ===================================================== *)
  (* sequentially generating random numbers *)


  for j = 1 to (iN-1) do
    for l = 0 to (iFactors-1) do
      (* compute random number in exact same sequence *)
      (* 10% of the total executition time *)
      randz.(l).(j) <- ranUnif lRndSeed
    done
  done ;

  (* ===================================================== *)
  (* shocks to hit various factors for forward curve at t *)

  (* 18% of the total executition time *)
  (* serialB pdz randz blocksize iN iFactors ; *)
  for l = 0 to (iFactors-1) do
    for j = 1 to (iN-1) do
      pdz.(l).(j) <- cumNormalInv randz.(l).(j)
    done [@perforate]
  done ;



  (* ===================================================== *)
  (* Generation of HJM Path1 *)
  for j = 1 to (iN-1) do
    for l = 0 to (iN-(j+1)) do
      dTotalShock := 0. ;

      for i = 0 to (iFactors-1) do
        dTotalShock := !dTotalShock +. ppdFactors.(i).(l) *. pdz.(i).(j)
      done ;

      (* as per formula *)
      ppdHJMPath.(j).(l) <- ppdHJMPath.(j-1).(l+1) +. pdTotalDrift.(l)*.ddelt +. sqrt_ddelt *. !dTotalShock
    done [@perforate]
  done [@perforate] ;

  ppdHJMPath

let hjm_Swaption_Blocking
  dStrike
  dCompounding    (* //Compounding convention used for quoting the strike (0 => continuous, *)
                  (* //0.5 => semi-annual, 1 => annual). *)
  dMaturity       (* //Maturity of the swaption (time to expiration) *)
  dTenor          (* //Tenor of the swap *)
  dPaymentInterval(* //frequency of swap payments e.g. dPaymentInterval = 0.5 implies a swap payment every half *)
                  (* //year *)
                  (* //HJM Framework Parameters (please refer HJM.cpp for explanation of variables and functions) *)
  iN
  iFactors
  dYears
  pdYield
  ppdFactors
  iRndSeed
  lTrials
  tid =

  (* ddelt = HJM matrix time-step width. e.g. if dYears = 5yrs and *)
  let ddelt = dYears /. (float_of_int iN) in

  (* //iN = no. of time points = 10, then ddelt = step length = 0.5yrs *)
  (* = ratio of time gap between swap payments and HJM step-width. *)
  (* e.g. dPaymentInterval = 1 year. ddelt = 0.5year. This implies that a swap *)
  (* payment will be made after every 2 HJM time steps. *)
  let iFreqRatio = int_of_float (dPaymentInterval /. ddelt +. 0.5) in

  (* //Strike quoted in continuous compounding convention.  *)
  (* //As HJM rates are continuous, the K in max(R-K,0) will be dStrikeCont and not dStrike. *)
  let dStrikeCont = if dCompounding = 0. then
      (* by convention, dCompounding = 0 means that the strike entered by user has been quoted *)
      (* using continuous compounding convention *)
      dStrike
    else
      (* converting quoted strike to continuously compounded strike *)
      (1./.(dCompounding))*.log(1.+.dStrike*.(dCompounding)) in

  (* e.g., let k be strike quoted in semi-annual convention. Therefore, 1$ at the end of *)
  (* half a year would earn = (1+k/2). For converting to continuous compounding,  *)
  (* (1+0.5*k) = exp(K*0.5) *)
  (* => K = (1/0.5)*ln(1+0.5*k) *)


  (* HJM Framework vectors and matrices *)
  (* Length of the HJM rate path at the time index corresponding to swaption maturity. *)
  (* This is the length of the HJM rate path at the time index *)
  let iSwapVectorLength = int_of_float ((float_of_int iN) -. dMaturity/.ddelt +. 0.5) in

  let pdForward = ref [||] in
  let ppdDrifts = ref [||] in
  let pdTotalDrift = ref [||] in

  (* ******************************* *)
  pdForward := dvector 0 (iN-1) ;
  ppdDrifts := dmatrix 0 (iFactors-1) 0 (iN-2) ;
  pdTotalDrift := dvector 0 (iN-2) ;

  (* ================================== *)
  (* **** per Trial data **** *)
  (* payoff will be discounted *)
  (* payments made will be discounted    *)
  (* vector to store swap payoffs *)
  let pdSwapPayoffs = dvector 0 (iSwapVectorLength - 1) in


  let iSwapStartTimeIndex = ref 0 in
  let iSwapTimePoints = ref 0 in
  let dSwapVectorYears = ref 0. in

  (* ******************************* *)

  (* Accumulators *)
  let dSumSimSwaptionPrice = ref 0. in
  let dSumSquareSimSwaptionPrice = ref 0. in

  (* Swap starts at swaption maturity *)
  iSwapStartTimeIndex := int_of_float (dMaturity/.ddelt +. 0.5) ;
  (* Total HJM time points corresponding to the swap's tenor *)
  iSwapTimePoints := int_of_float (dTenor/.ddelt +. 0.5) ;
  dSwapVectorYears := (float_of_int iSwapVectorLength)*.ddelt ;



  (* now we store the swap payoffs in the swap payoff vector *)
  let i = ref iFreqRatio in
  while !i <= !iSwapTimePoints do
    pdSwapPayoffs.(!i) <- if !i != !iSwapTimePoints then
        (* the bond pays coupon equal to this amount *)
        exp (dStrikeCont*.dPaymentInterval) -. 1.
      else
        (* at terminal time point, bond pays coupon plus par amount *)
        exp (dStrikeCont*.dPaymentInterval) ;
    i := !i + iFreqRatio
  done ;

  (* generating forward curve at t=0 from supplied yield curve *)
  hjm_Yield_to_Forward !pdForward iN pdYield ;

  (* computation of drifts from factor volatilities *)
  hjm_Drifts !pdTotalDrift !ppdDrifts iN iFactors dYears ppdFactors ;

  dSumSimSwaptionPrice := 0.0 ;
  dSumSquareSimSwaptionPrice := 0.0 ;

  let seedref = ref iRndSeed in

  (* Simulations begin: *)
  let l = ref 0 in
  while !l <= lTrials-1 do
  (* For each trial a new HJM Path is generated *)
    (* GC: 51% of the time goes here *)
    let ppdHJMPath = hjm_SimPath_Forward_Blocking iN iFactors  dYears  !pdForward  !pdTotalDrift ppdFactors seedref in

    (* now we compute the discount factor vector *)

    let pdDiscountingRatePath = Array.map (fun a -> a.(0)) ppdHJMPath in

    (* 15% of the time goes here *)
    let pdPayoffDiscountFactors = discount_Factors_Blocking iN dYears pdDiscountingRatePath in

    let pdSwapRatePath = Array.init iSwapVectorLength (fun i -> ppdHJMPath.(!iSwapStartTimeIndex).(i)) in

    let pdSwapDiscountFactors = discount_Factors_Blocking iSwapVectorLength !dSwapVectorYears pdSwapRatePath in

    (* ======================== *)
    (* Simulation *)
    let dFixedLegValue = ref 0.0 in
    for i = 0 to (iSwapVectorLength-1) do
      dFixedLegValue := !dFixedLegValue +. pdSwapPayoffs.(i)*. pdSwapDiscountFactors.(i)
    done ;
    let dSwaptionPayoff = dmax (!dFixedLegValue -. 1.0) 0. in

    let dDiscSwaptionPayoff = dSwaptionPayoff*. pdPayoffDiscountFactors.(!iSwapStartTimeIndex) in

    (* ========= end simulation ====================================== *)

    (* accumulate into the aggregating variables ===================== *)
    dSumSimSwaptionPrice := !dSumSimSwaptionPrice +. dDiscSwaptionPayoff ;
    dSumSquareSimSwaptionPrice := !dSumSquareSimSwaptionPrice +. dDiscSwaptionPayoff *. dDiscSwaptionPayoff ;

    l := !l + 1
  done ;

  (* Simulation Results Stored *)
  let dSimSwaptionMeanPrice = !dSumSimSwaptionPrice/.(float_of_int lTrials) in
  let dSimSwaptionStdError = sqrt
      ((!dSumSquareSimSwaptionPrice-. !dSumSimSwaptionPrice*. !dSumSimSwaptionPrice/.(float_of_int lTrials))
       /.
       (float_of_int (lTrials-1))
      )
      /. sqrt (float_of_int lTrials) in

  (* results returned *)
  dSimSwaptionMeanPrice, dSimSwaptionStdError


(*  ================================================= *)

let dSumSimSwaptionPrice_global = ref 0.0

let dSumSquareSimSwaptionPrice_global = ref 0.0

let chunksize = ref 0

let worker () =

  for i = 0 to (!nSwaptions-1) do
    let price, error = hjm_Swaption_Blocking
      !swaptions.(i).dStrike
      !swaptions.(i).dCompounding
      !swaptions.(i).dMaturity
      !swaptions.(i).dTenor
      !swaptions.(i).dPaymentInterval
      !swaptions.(i).iN
      !swaptions.(i).iFactors
      !swaptions.(i).dYears
      !swaptions.(i).pdYield
      !swaptions.(i).ppdFactors
      (Int64.(add !swaption_seed (of_int i)))
      !num_trials 0 in
    !swaptions.(i).dSimSwaptionMeanPrice <- price ;
    !swaptions.(i).dSimSwaptionStdError <- error ;
  done



(* print a little help message explaining how to use this program *)
let print_usage name =
  Printf.eprintf "Usage: %s OPTION [OPTIONS]...\n" name ;
  Printf.eprintf "Options:\n" ;
  Printf.eprintf "\t-ns [number of swaptions (should be > number of threads]\n" ;
  Printf.eprintf "\t-sm [number of simulations]\n" ;
  Printf.eprintf "\t-nt [number of threads]\n" ;
  Printf.eprintf "\t-sd [random number seed]\n"

(* Please note: Whenever we type-cast to (int), we add 0.5 to ensure that the value is rounded to the correct number. *)
(* For instance, if X/Y = 0.999 then (int) (X/Y) will equal 0 and not 1 (as (int) rounds down). *)
(* Adding 0.5 ensures that this does not happen. Therefore we use (int) (X/Y + 0.5); instead of (int) (X/Y); *)

let main argc argv =
  let factors = ref [||] in

  (* if argc = 1 then (print_usage(argv.(0)) ; exit 1) ; *)

  Arg.parse
    (let open Arg in
     align [
       "-ns", Int (fun i -> nSwaptions := i), "number of swaptions" ;
       "-sm", Int (fun i -> num_trials := i), "number of simulations" ;
       "-sd", Int (fun i -> seed := Int64.of_int i), "random number seed"
     ])
    (fun _ -> failwith "no anon args allowed")
    "run with no arguments to see usage" ;

  nSwaptions := 1000 ;
  num_trials := 1000 ;
  seed := Int64.of_int 1 ;

  Printf.printf "Number of Simulations: %d,  Number of swaptions: %d\n" !num_trials !nSwaptions ;
  swaption_seed := Int64.of_float (2147483647. *. ranUnif seed) ;

  (* initialize input dataset *)
  factors := dmatrix 0 (iFactors-1) 0 (iN-2) ;
  (* the three rows store vol data for the three factors *)
  !factors.(0).(0) <- 0.01 ;
  !factors.(0).(1) <- 0.01 ;
  !factors.(0).(2) <- 0.01 ;
  !factors.(0).(3) <- 0.01 ;
  !factors.(0).(4) <- 0.01 ;
  !factors.(0).(5) <- 0.01 ;
  !factors.(0).(6) <- 0.01 ;
  !factors.(0).(7) <- 0.01 ;
  !factors.(0).(8) <- 0.01 ;
  !factors.(0).(9) <- 0.01 ;

  !factors.(1).(0) <- 0.009048 ;
  !factors.(1).(1) <- 0.008187 ;
  !factors.(1).(2) <- 0.007408 ;
  !factors.(1).(3) <- 0.006703 ;
  !factors.(1).(4) <- 0.006065 ;
  !factors.(1).(5) <- 0.005488 ;
  !factors.(1).(6) <- 0.004966 ;
  !factors.(1).(7) <- 0.004493 ;
  !factors.(1).(8) <- 0.004066 ;
  !factors.(1).(9) <- 0.003679 ;

  !factors.(2).(0) <- 0.001000 ;
  !factors.(2).(1) <- 0.000750 ;
  !factors.(2).(2) <- 0.000500 ;
  !factors.(2).(3) <- 0.000250 ;
  !factors.(2).(4) <- 0.000000 ;
  !factors.(2).(5) <- -0.000250 ;
  !factors.(2).(6) <- -0.000500 ;
  !factors.(2).(7) <- -0.000750 ;
  !factors.(2).(8) <- -0.001000 ;
  !factors.(2).(9) <- -0.001250 ;

  (* setting up multiple swaptions *)
  swaptions := Array.init !nSwaptions (fun i ->
      let the_yield = dvector 0 (iN-1) in
      the_yield.(0) <- 0.1 ;
      for j = 1 to (iN-1) do
          the_yield.(j) <- the_yield.(j-1)+. 0.005
      done ;

      let the_ppdFactors = dmatrix 0 (iFactors-1) 0 (iN-2) in
      for k = 0 to (iFactors-1) do
        for j = 0 to (iN-2) do
          the_ppdFactors.(k).(j) <- !factors.(k).(j)
        done
      done ;

      { id = Int64.of_int i
      ; iN = iN
      ; iFactors = iFactors
      (* 5 to 20 years in 3 month intervals *)
      ; dYears = 5.0 +. ((60. *. ranUnif seed))*.0.25
      (* strikes ranging from 0.1 to 5.0 in steps of 0.1 *)
      ; dStrike = 0.1 +. (float_of_int (int_of_float (49. *. ranUnif seed)))*.0.1
      ; dCompounding = 0.
      ; dMaturity = 1.0
      ; dTenor = 2.0
      ; dPaymentInterval = 1.0
      ; pdYield = the_yield
      ; ppdFactors = the_ppdFactors

      ; dSimSwaptionMeanPrice = 0.
      ; dSimSwaptionStdError = 0.
      }
    ) ;

  worker () ;

  let fout = open_out (Sys.argv.(0) ^ ".out") in

  for i = 0 to (!nSwaptions-1) do
    Printf.fprintf fout "Swaption %d: [SwaptionPrice: %.10f StdError: %.10f] \n"
            i !swaptions.(i).dSimSwaptionMeanPrice !swaptions.(i).dSimSwaptionStdError
  done ;

  close_out fout

let () =
  main (Array.length Sys.argv) Sys.argv
