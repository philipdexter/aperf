type config = float list
type config_result =
  { conf : config
  ; time : float
  ; speedup : float
  ; accuracy_loss : float
  ; score : float
  }

module type T = sig
  (** num_loops -> config_run_function -> list of configs and their results *)
  val run : int -> (config -> config_result) -> (config * config_result) list
end

module Exhaustive : T

module HillClimb : T

