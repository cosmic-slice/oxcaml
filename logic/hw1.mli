open! Core

val init_beads : int
val num_squares_per_side : int

type players =
  | PlayerOne
  | PlayerTwo

type decision =
  | Playing of { whose_turn : players }
  | Winner of players
  | Tie

type game_state =
  { board : int array
  ; num_squares_per_side : int
  ; decision : decision
  ; last_move : int option
  }

val get_init_board : int -> int -> int array
val initial_state : game_state
val first_move : int
val state_after_first_move : game_state
val before_terminal_state : game_state
val move_to_terminal_state : int
val terminal_state : game_state