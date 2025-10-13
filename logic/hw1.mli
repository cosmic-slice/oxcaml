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
  { player_one_side: int array
  ; player_two_side: int array
  ; player_one_score: int
  ; player_two_score: int
  ; decision : decision
  }

type move = int

val initial_state : game_state
val first_move : move
val state_after_first_move : game_state
val before_terminal_state : game_state
val move_to_terminal_state : move
val terminal_state : game_state
