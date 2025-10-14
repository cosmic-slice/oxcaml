open! Core
open! Stdlib

let init_beads = 4
let num_squares_per_side = 6

type players =
  | PlayerOne
  | PlayerTwo

type decision =
  | Playing of { whose_turn : players }
  | Winner of players
  | Tie

type game_state =
  { player_one_side : int array
  ; player_two_side : int array
  ; player_one_score : int
  ; player_two_score : int
  ; decision : decision
  }

type move = int

(*=
Initial state of a Mancala game. PlayerOne is the top row, PlayerTwo is the bottom

  4 4 4 4 4 4
0             0
  4 4 4 4 4 4
*)
let initial_state : game_state =
  { player_one_side = Array.make num_squares_per_side init_beads
  ; player_two_side = Array.make num_squares_per_side init_beads
  ; player_one_score = 0
  ; player_two_score = 0
  ; decision = Playing { whose_turn = PlayerOne }
  }
;;

let first_move : move = 4

(*=
Second state after PlayerOne moves beads from square 4 

  5 5 5 0 4 4
1             0
  4 4 4 4 4 4

PlayerOne gets another turn because the last bead landed in his own goal
*)
let state_after_first_move : game_state =
  { player_one_side = [| 5; 5; 5; 0; 4; 4 |]
  ; player_two_side = [| 4; 4; 4; 4; 4; 4 |]
  ; player_one_score = 1
  ; player_two_score = 0
  ; decision = Playing { whose_turn = PlayerOne }
  }
;;

(*=
State before PlayerOne makes final move that ends the game

   1 0 0 0 0 0
16             23
   2 0 4 0 1 1
*)
let before_terminal_state : game_state =
  { player_one_side = [| 1; 0; 0; 0; 0; 0 |]
  ; player_two_side = [| 2; 0; 4; 0; 1; 1 |]
  ; player_one_score = 16
  ; player_two_score = 23
  ; decision = Playing { whose_turn = PlayerOne }
  }
;;

let move_to_terminal_state : move = 1

(*=
The final state of the game, where PlayerTwo has won

   0 0 0 0 0 0
17             31
   0 0 0 0 0 0
*)
let terminal_state : game_state =
  { player_one_side = [| 0; 0; 0; 0; 0; 0 |]
  ; player_two_side = [| 0; 0; 0; 0; 0; 0 |]
  ; player_one_score = 17
  ; player_two_score = 31
  ; decision = Winner PlayerTwo
  }
;;
