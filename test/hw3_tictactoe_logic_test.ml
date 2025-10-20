open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic

let ok_exn result = Result.ok result |> Option.value_exn

let%test "Unit test for initializing Mancala board (returns bool)" =
  let state = Game_state.create ~num_squares_per_side:6 ~init_beads:4 |> ok_exn in
  let expected_state : Game_state.t =
    { board = [| 0; 4; 4; 4; 4; 4; 4; 0; 4; 4; 4; 4; 4; 4 |]
    ; num_squares_per_side = 6
    ; decision = Playing { whose_turn = Players.PlayerOne }
    ; last_move = None (* For animation purposes. *)
    }
  in
  Game_state.equal state expected_state
;;

let create_and_print ~num_squares_per_side ~init_beads =
  let result = Game_state.create ~num_squares_per_side ~init_beads in
  print_s [%sexp (result : (Game_state.t, Game_state.Create_error.t list) Result.t)]
;;

let%expect_test "Example of an expect_test (returns unit)" =
  create_and_print ~num_squares_per_side:6 ~init_beads:4;
  [%expect
    {|
    (Ok
     ((board (0 4 4 4 4 4 4 0 4 4 4 4 4 4)) (num_squares_per_side 6)
      (decision (Playing (whose_turn PlayerOne))) (last_move ())))
    |}]
;;

let%expect_test "Game_state.create fails on big (and small) sizes" =
  create_and_print ~num_squares_per_side:6 ~init_beads:4;
  [%expect
    {|
    (Ok
     ((board (0 4 4 4 4 4 4 0 4 4 4 4 4 4)) (num_squares_per_side 6)
      (decision (Playing (whose_turn PlayerOne))) (last_move ())))
    |}];
  create_and_print ~num_squares_per_side:3 ~init_beads:4;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~num_squares_per_side:30 ~init_beads:30;
  [%expect {| (Error (Board_too_big_or_small Bead_count_invalid)) |}];
  create_and_print ~num_squares_per_side:(-30) ~init_beads:(-30);
  [%expect {| (Error (Board_too_big_or_small Bead_count_invalid)) |}];
  create_and_print ~num_squares_per_side:50 ~init_beads:4;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~num_squares_per_side:0 ~init_beads:4;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~num_squares_per_side:(-20) ~init_beads:4;
  [%expect {| (Error (Board_too_big_or_small)) |}];
  create_and_print ~num_squares_per_side:6 ~init_beads:0;
  [%expect {| (Error (Bead_count_invalid)) |}];
  create_and_print ~num_squares_per_side:6 ~init_beads:25;
  [%expect {| (Error (Bead_count_invalid)) |}];
  create_and_print ~num_squares_per_side:6 ~init_beads:(-7);
  [%expect {| (Error (Bead_count_invalid)) |}]
;;

(*let%expect_test "Game_state.all_directions" =
  print_s [%sexp (Game_state.For_testing.all_directions : (int * int) list)];
  [%expect {| ((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)) |}]
;;*)

let make_move_and_print game_state cell_position =
  let result = Game_state.make_move game_state cell_position in
  print_s [%sexp (result : (Game_state.t, Game_state.Move_error.t) Result.t)]
;;

let initial_standard_board =
  Game_state.create ~num_squares_per_side:6 ~init_beads:4 |> ok_exn
;;

(*let initial_big_board =
  Game_state.create ~num_squares_per_side:10 ~init_beads:10 |> ok_exn
;;*)

let%expect_test "Game_state.make_move from PlayerOne's cell 5 on standard board" =
  make_move_and_print initial_standard_board 5;
  [%expect
    {|
    (Ok
     ((board (0 4 4 4 4 4 4 0 4 0 5 5 5 5)) (num_squares_per_side 6)
      (decision (Playing (whose_turn PlayerTwo))) (last_move (5))))
    |}]
;;

let%expect_test
    "Game_state.make_move from PlayerOne's cell 4 on standard board and get an extra turn"
  =
  make_move_and_print initial_standard_board 4;
  [%expect
    {|
    (Ok
     ((board (1 4 4 4 4 4 4 0 4 4 0 5 5 5)) (num_squares_per_side 6)
      (decision (Playing (whose_turn PlayerOne))) (last_move (4))))
    |}]
;;

let%expect_test "Game_state.make_move fails for various invalid positions" =
  make_move_and_print initial_standard_board 7;
  [%expect {| (Error Not_a_valid_square) |}];
  make_move_and_print initial_standard_board (-1);
  [%expect {| (Error Not_a_valid_square) |}];
  make_move_and_print initial_standard_board 0;
  [%expect {| (Error Not_a_valid_square) |}]
;;

let%expect_test "Game_state.make_move fails if player chooses an empty square" =
  let first_move = 4 in
  let state_after_first_move =
    Game_state.make_move initial_standard_board first_move |> ok_exn
  in
  make_move_and_print state_after_first_move first_move;
  [%expect {| (Error Square_is_empty) |}]
;;

let%expect_test "Game_state.make_move where PlayerOne steals a bead" =
  let steal_move = 4 in
  let pre_steal_state : Game_state.t = 
    { board = [|23; 0; 0; 1; 0; 0; 0; 23; 0; 0; 1; 0; 0; 0|]
    ; num_squares_per_side = 6
    ; decision = Playing { whose_turn = Players.PlayerOne}
    ; last_move = Some 6
  } in
  make_move_and_print pre_steal_state steal_move;
  [%expect 
  {|
    (Ok
     ((board (25 0 0 0 0 0 0 23 0 0 0 0 0 0)) (num_squares_per_side 6)
      (decision (Winner PlayerOne)) (last_move (4)))) |}]
;;

let pretty_print_board (state : Game_state.t) =
  (* Get each player's score *)
  let score_1 = string_of_int (Game_state.get_score state Players.PlayerOne) in
  let score_2 = string_of_int (Game_state.get_score state Players.PlayerTwo) in
  
  (* Initialize row string variables and halfway point *)
  let halfway_index = (Array.length state.board / 2) - 1 in
  let bottom_row = ref "" in
  let top_row = ref "" in

  (* Pad top and bottom rows based on length of PlayerOne's score *)
  top_row := !top_row ^ String.make (String.length score_1 ) ' ' ^ " ";
  bottom_row := !bottom_row ^ String.make (String.length score_1) ' ' ^ " ";
  
  (* Iterate through elements on each side of the array *)
  for i = 1 to halfway_index do
    (* Get next element of row and append it to the strings *)
    let bottom_val = string_of_int state.board.(i) ^ " " in
    let top_val = string_of_int state.board.(Array.length state.board - i) ^ " " in
    
    bottom_row := !bottom_row ^ bottom_val;
    top_row := !top_row ^ top_val;
    
    (* Add padding to the shorter row so they have the same length *)
    let length_diff = String.length !top_row - String.length !bottom_row in
    if length_diff > 0 then
      bottom_row := !bottom_row ^ String.make length_diff ' '
    else if length_diff < 0 then
      top_row := !top_row ^ String.make (-length_diff) ' '
  done;
  
  let middle_row = score_1 ^ String.make (String.length !top_row - String.length score_1) ' ' ^ score_2 in
  
  (* Print all of the rows from top to bottom *)
  print_endline !top_row;
  print_endline middle_row;
  print_endline !bottom_row;
  print_s [%sexp (state.decision : Decision.t)]
;;

let print_final_state game_state move_sequence =
  let result =
    List.fold move_sequence ~init:game_state ~f:(fun new_state move ->
      Game_state.make_move new_state move |> ok_exn)
  in
  pretty_print_board result
;;

let%expect_test "Game_state.make_move: PlayerOne moves beads from square 4" =
  print_final_state initial_standard_board [ 4 ];
  [%expect
    {|
      5 5 5 0 4 4
    1             0
      4 4 4 4 4 4
    (Playing (whose_turn PlayerOne))
    |}]
;;

(*let%expect_test "Game_state.make_move: X makes a move, then O makes a move" =
  print_final_state initial_3x3 [ { row = 1; column = 1 }; { row = 0; column = 0 } ];
  [%expect
    {|
    O| |
    -----
     |X|
    -----
     | |
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe X wins vertically" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 2 }
    ; { row = 1; column = 0 }
    ; { row = 1; column = 2 }
    ; { row = 1; column = 1 }
    ; { row = 2; column = 2 }
    ];
  [%expect
    {|
     | |X
    -----
    O|O|X
    -----
     | |X
    (Winner X)
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe X wins horizontally" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 0 }
    ; { row = 1; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 1; column = 1 }
    ; { row = 0; column = 2 }
    ];
  [%expect
    {|
    X|X|X
    -----
    O|O|
    -----
     | |
    (Winner X)
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe O wins horizontally" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 0 }
    ; { row = 1; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 1; column = 1 }
    ; { row = 2; column = 2 }
    ; { row = 1; column = 2 }
    ];
  [%expect
    {|
    X|X|
    -----
    O|O|O
    -----
     | |X
    (Winner O)
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe O wins diagonally" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 0 }
    ; { row = 2; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 1; column = 1 }
    ; { row = 2; column = 2 }
    ; { row = 0; column = 2 }
    ];
  [%expect
    {|
    X|X|O
    -----
     |O|
    -----
    O| |X
    (Winner O)
    |}]
;;

let%expect_test "Game_state.make_move: tictactoe stalemate" =
  print_final_state
    initial_3x3
    [ { row = 0; column = 0 }
    ; { row = 1; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 1; column = 1 }
    ; { row = 2; column = 0 }
    ; { row = 2; column = 1 }
    ; { row = 1; column = 2 }
    ; { row = 0; column = 2 }
    ; { row = 2; column = 2 }
    ];
  [%expect
    {|
    X|X|O
    -----
    O|O|X
    -----
    X|O|X
    Stalemate
    |}]
;;

let%expect_test "Game_state.make_move: full gomoku game until O wins" =
  print_final_state
    initial_gomoku
    [ { row = 0; column = 0 }
    ; { row = 1; column = 0 }
    ; { row = 0; column = 1 }
    ; { row = 2; column = 1 }
    ; { row = 0; column = 2 }
    ; { row = 3; column = 2 }
    ; { row = 0; column = 3 }
    ; { row = 4; column = 3 }
    ; { row = 0; column = 9 }
    ; { row = 5; column = 4 }
    ];
  [%expect
    {|
    X|X|X|X| | | | | |X| | | | |
    -----------------------------
    O| | | | | | | | | | | | | |
    -----------------------------
     |O| | | | | | | | | | | | |
    -----------------------------
     | |O| | | | | | | | | | | |
    -----------------------------
     | | |O| | | | | | | | | | |
    -----------------------------
     | | | |O| | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    -----------------------------
     | | | | | | | | | | | | | |
    (Winner O)
    |}]
;;

let%expect_test "Game_state.get_all_moves for tictactoe" =
  let all_moves = Game_state.get_all_moves initial_3x3 in
  print_s [%message "All moves for 3x3 board" (all_moves : Move.t list)];
  [%expect
    {|
    ("All moves for 3x3 board"
     (all_moves
      (((row 0) (column 0)) ((row 0) (column 1)) ((row 0) (column 2))
       ((row 1) (column 0)) ((row 1) (column 1)) ((row 1) (column 2))
       ((row 2) (column 0)) ((row 2) (column 1)) ((row 2) (column 2)))))
    |}]
;;

let random_walk (initial_state : Game_state.t) ~random_seed =
  let rec random_walk (state : Game_state.t) =
    let all_moves = Game_state.get_all_moves state in
    let next_states =
      List.filter_map all_moves ~f:(fun move ->
        Game_state.make_move state move |> Result.ok)
    in
    let random_state = List.random_element next_states |> Option.value_exn in
    match Decision.is_game_over random_state.decision with
    | true -> random_state
    | false -> random_walk random_state
  in
  (* Set random seed. *)
  Core.Random.init random_seed;
  pretty_print_board (random_walk initial_state)
;;

let%expect_test "TicTacToe random walk till terminal state" =
  random_walk initial_3x3 ~random_seed:1;
  [%expect
    {|
    O|O|X
    -----
     |X|
    -----
    X|O|X
    (Winner X)
    |}];
  random_walk initial_3x3 ~random_seed:3;
  [%expect
    {|
    X|X|O
    -----
    O|O|X
    -----
    X|X|O
    Stalemate
    |}];
  random_walk initial_3x3 ~random_seed:1234;
  [%expect
    {|
    X| |O
    -----
    X| |O
    -----
    X|O|X
    (Winner X)
    |}];
  random_walk initial_gomoku ~random_seed:1;
  [%expect
    {|
     | |O| |X|X|X| | | |O| | |X|
    -----------------------------
    O|O| | | |O| | |X|X|X|O|X| |X
    -----------------------------
    X| | | |O|X| |X| | | | | | |X
    -----------------------------
     | | |O| |X|O| | | |X| | | |X
    -----------------------------
     | | |O| | | |O|O| | |O| | |X
    -----------------------------
    O|X|X|X|X|X| | |O| | |X| | |
    -----------------------------
     |O| | | | | |O| |O|O|O|O| |X
    -----------------------------
    X| | |O| |O|O| |X| | | | |O|O
    -----------------------------
     | | |O| |X|O|O| |O|X| | | |
    -----------------------------
    O| |X| |O|O| | | | | | | | |
    -----------------------------
     |X| |O| | | |X|O| |X| | |X|
    -----------------------------
     | | | |X|X| | |O| |X|O| |X|
    -----------------------------
     |O| | | | | | |O|X|X|X| | |
    -----------------------------
    O| |X| |X|X| | |X| | |O| | |O
    -----------------------------
     |O|O|X| | |X| | | | | | | |O
    (Winner X)
    |}]
;; *)
