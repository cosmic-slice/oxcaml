open! Core
open Mancala_logic_library
open Hw2_mancala_logic

val ok_exn : ('a, 'b) result -> 'a
val create_and_print : num_squares_per_side:int -> init_beads:int -> unit
val make_move_and_print : Game_state.t -> int -> unit
val pretty_print_board : Game_state.t -> unit
val print_final_state : Game_state.t -> int list -> unit
val random_walk : Game_state.t -> random_seed:int -> unit
