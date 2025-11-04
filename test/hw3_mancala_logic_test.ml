open! Core
open Mancala_logic_library
open Hw2_mancala_logic

let ok_exn result = Result.ok result |> Option.value_exn

let%test "Unit test for initializing Mancala board (returns bool)" =
  let state = Game_state.create ~num_squares_per_side:6 ~init_beads:4 |> ok_exn in
  let expected_state : Game_state.t =
    { board = [| 0; 4; 4; 4; 4; 4; 4; 0; 4; 4; 4; 4; 4; 4 |]
    ; num_squares_per_side = 6
    ; decision = Playing { whose_turn = Players.PlayerOne }
    ; last_move = None, None (* For animation purposes. *)
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
      (decision (Playing (whose_turn PlayerOne))) (last_move (() ()))))
    |}]
;;

let%expect_test "Game_state.create fails on big (and small) sizes" =
  create_and_print ~num_squares_per_side:6 ~init_beads:4;
  [%expect
    {|
    (Ok
     ((board (0 4 4 4 4 4 4 0 4 4 4 4 4 4)) (num_squares_per_side 6)
      (decision (Playing (whose_turn PlayerOne))) (last_move (() ()))))
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

let initial_big_board =
  Game_state.create ~num_squares_per_side:10 ~init_beads:10 |> ok_exn
;;

let%expect_test "Game_state.make_move from PlayerOne's cell 5 on standard board" =
  make_move_and_print initial_standard_board 5;
  [%expect
    {|
    (Ok
     ((board (0 4 4 4 4 4 4 0 4 0 5 5 5 5)) (num_squares_per_side 6)
      (decision (Playing (whose_turn PlayerTwo))) (last_move ((PlayerOne) (5)))))
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
      (decision (Playing (whose_turn PlayerOne))) (last_move ((PlayerOne) (4)))))
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
    { board = [| 23; 0; 0; 1; 0; 0; 0; 23; 0; 0; 1; 0; 0; 0 |]
    ; num_squares_per_side = 6
    ; decision = Playing { whose_turn = Players.PlayerOne }
    ; last_move = Some PlayerOne, Some 6
    }
  in
  make_move_and_print pre_steal_state steal_move;
  [%expect
    {|
    (Ok
     ((board (25 0 0 0 0 0 0 23 0 0 0 0 0 0)) (num_squares_per_side 6)
      (decision (Winner PlayerOne)) (last_move ((PlayerOne) (4))))) |}]
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
  top_row := !top_row ^ String.make (String.length score_1) ' ' ^ " ";
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
    if length_diff > 0
    then bottom_row := !bottom_row ^ String.make length_diff ' '
    else if length_diff < 0
    then top_row := !top_row ^ String.make (-length_diff) ' '
  done;
  let middle_row =
    score_1 ^ String.make (String.length !top_row - String.length score_1) ' ' ^ score_2
  in
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

let%expect_test "Game_state.make_move: PlayerOne makes two moves in a row" =
  print_final_state initial_standard_board [ 4; 5 ];
  [%expect
    {|
      6 6 6 1 0 4
    1             0
      4 4 4 4 4 4
    (Playing (whose_turn PlayerTwo))
    |}]
;;

let%expect_test
    "Game_state.make_move: Shortest possible game (10 moves) where PlayerOne wins"
  =
  print_final_state initial_standard_board [ 4; 1; 2; 3; 6; 4; 6; 5; 6; 6 ];
  [%expect
    {|
       0 0 0 0 0 0
    41             7
       0 0 0 0 0 0
    (Winner PlayerOne)
    |}]
;;

let%expect_test "Game_state.make_move: A game where PlayerTwo wins" =
  print_final_state
    initial_standard_board
    [ 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 1
    ; 2
    ; 4
    ; 5
    ; 6
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 1
    ; 3
    ; 4
    ; 6
    ; 1
    ; 4
    ; 2
    ; 1
    ; 3
    ; 3
    ; 6
    ; 4
    ];
  [%expect
    {|
       0 0 0 0 0 0
    21             27
       0 0 0 0 0 0
    (Winner PlayerTwo)
    |}]
;;

let%expect_test "Game_state.make_move: A game that ends in a tie" =
  print_final_state
    initial_standard_board
    [ 2
    ; 5
    ; 3
    ; 3
    ; 1
    ; 3
    ; 4
    ; 3
    ; 6
    ; 2
    ; 4
    ; 6
    ; 5
    ; 5
    ; 3
    ; 3
    ; 2
    ; 2
    ; 1
    ; 6
    ; 3
    ; 1
    ; 2
    ; 3
    ; 5
    ; 4
    ; 4
    ; 1
    ; 3
    ; 5
    ; 5
    ; 6
    ; 3
    ; 3
    ; 6
    ; 6
    ; 4
    ];
  [%expect
    {|
       0 0 0 0 0 0
    24             24
       0 0 0 0 0 0
    Tie
    |}]
;;

let%expect_test "Game_state.make_move: Test case for huge mancala board" =
  print_final_state
    initial_big_board
    [ 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 7
    ; 8
    ; 9
    ; 10
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 7
    ; 8
    ; 9
    ; 10
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 7
    ; 8
    ; 9
    ; 10
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 7
    ; 8
    ; 9
    ; 10
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 7
    ; 8
    ; 9
    ; 10
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 6
    ; 7
    ; 8
    ; 9
    ; 10
    ; 1
    ; 2
    ; 3
    ; 4
    ; 6
    ; 1
    ; 2
    ; 5
    ; 1
    ; 2
    ; 3
    ; 4
    ; 5
    ; 3
    ; 1
    ];
  [%expect
    {|
       0 1 0 1 0 0  15 2  13 1
    50                          63
       2 1 0 1 5 17 4  11 2  11
    (Playing (whose_turn PlayerOne))
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

let%expect_test "Mancala random walk till terminal state" =
  random_walk initial_standard_board ~random_seed:1;
  [%expect
    {|
       0 0 0 0 0 0
    27             21
       0 0 0 0 0 0
    (Winner PlayerOne)
    |}];
  random_walk initial_standard_board ~random_seed:5;
  [%expect
    {|
       0 0 0 0 0 0
    23             25
       0 0 0 0 0 0
    (Winner PlayerTwo)
    |}];
  random_walk initial_standard_board ~random_seed:1234;
  [%expect
    {|
       0 0 0 0 0 0
    25             23
       0 0 0 0 0 0
    (Winner PlayerOne)
    |}];
  random_walk initial_big_board ~random_seed:1;
  [%expect
    {|
       0 0 0 0 0 0 0 0 0 0
    89                     111
       0 0 0 0 0 0 0 0 0 0
    (Winner PlayerTwo)
    |}]
;;
