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
  { board : int array
  ; num_squares_per_side : int
  ; decision : decision
  ; last_move : int option
  }

let get_init_board num_squares_per_side init_beads =
  let board = Array.make ((2 * num_squares_per_side) + 2) init_beads in
  board.(0) <- 0;
  board.(num_squares_per_side + 1) <- 0;
  board
;;

(* type move = int *)

(*=
Initial state of a Mancala game. PlayerOne is the top row, PlayerTwo is the bottom

  4 4 4 4 4 4
0             0
  4 4 4 4 4 4
*)
let initial_state : game_state =
  { board = get_init_board num_squares_per_side init_beads
  ; num_squares_per_side = 6
  ; decision = Playing { whose_turn = PlayerOne }
  ; last_move = None
  }
;;

let first_move : int = 4

(*=
Second state after PlayerOne moves beads from square 4 

  5 5 5 0 4 4
1             0
  4 4 4 4 4 4

PlayerOne gets another turn because the last bead landed in his own goal
*)
let state_after_first_move : game_state =
  { board = [| 1; 4; 4; 4; 4; 4; 4; 0; 4; 4; 0; 5; 5; 5 |]
  ; num_squares_per_side = 6
  ; decision = Playing { whose_turn = PlayerOne }
  ; last_move = Some first_move
  }
;;

(*=
State before PlayerOne makes final move that ends the game

   1 0 0 0 0 0
16             23
   2 0 4 0 1 1
*)
let before_terminal_state : game_state =
  { board = [| 16; 2; 0; 4; 0; 1; 1; 23; 0; 0; 0; 0; 0; 1 |]
  ; num_squares_per_side = 6
  ; decision = Playing { whose_turn = PlayerOne }
  ; last_move = Some 4
  }
;;

let move_to_terminal_state : int = 1

(*=
The final state of the game, where PlayerTwo has won

   0 0 0 0 0 0
17             31
   0 0 0 0 0 0
*)
let terminal_state : game_state =
  { board = [| 17; 0; 0; 0; 0; 0; 0; 31; 0; 0; 0; 0; 0; 0 |]
  ; num_squares_per_side = 6
  ; decision = Winner PlayerTwo
  ; last_move = Some move_to_terminal_state
  }
;;