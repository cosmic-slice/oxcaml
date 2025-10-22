open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Hw4_alpha_beta_search
open Hw3_tictactoe_logic_test

let print_computer_move board turn max_depth =
  let state : Game_state.t =
    { board
    ; num_squares_per_side = 6
    ; decision = Playing { whose_turn = turn }
    ; last_move = (None, None)
    }
  in
  let move = alpha_beta state ~depth:max_depth |> Option.value_exn in
  let next_state = Game_state.make_move state move |> ok_exn in
  print_s [%message "Computer chooses this move" (move : int)];
  print_endline "\nThis transitions the game from this state:";
  pretty_print_board state;
  print_endline "\nTo this state:";
  pretty_print_board next_state
;;

let rec print_full_game (state : Game_state.t) max_depth index =
  let still_going =
    match state.decision with
    | Playing _ -> true
    | _ -> false
  in
  if still_going && index > 0
  then (
    let move = alpha_beta state ~depth:max_depth |> Option.value_exn in
    let next_state = Game_state.make_move state move |> ok_exn in
    print_s [%message "Computer chooses this move" (move : int) (index : int)];
    print_endline "\nThis transitions the game from this state:";
    pretty_print_board state;
    print_endline "\nTo this state:";
    pretty_print_board next_state;
    print_full_game next_state max_depth (index - 1))
;;

let%expect_test "Simulation for 5 moves with depth 3" =
  let init_state : Game_state.t =
    { board = [| 0; 4; 4; 4; 4; 4; 4; 0; 4; 4; 4; 4; 4; 4 |]
    ; num_squares_per_side = 6
    ; decision = Playing { whose_turn = Players.PlayerOne }
    ; last_move = None, None (* For animation purposes. *)
    }
  in
  print_full_game init_state 3 5;
  [%expect {| 
    ("Computer chooses this move" (move 4) (index 5))

    This transitions the game from this state:
      4 4 4 4 4 4
    0             0
      4 4 4 4 4 4
    (Playing (whose_turn PlayerOne))

    To this state:
      5 5 5 0 4 4
    1             0
      4 4 4 4 4 4
    (Playing (whose_turn PlayerOne))
    ("Computer chooses this move" (move 1) (index 4))

    This transitions the game from this state:
      5 5 5 0 4 4
    1             0
      4 4 4 4 4 4
    (Playing (whose_turn PlayerOne))

    To this state:
      0 5 5 0 4 4
    2             0
      5 5 5 5 4 4
    (Playing (whose_turn PlayerTwo))
    ("Computer chooses this move" (move 1) (index 3))

    This transitions the game from this state:
      0 5 5 0 4 4
    2             0
      5 5 5 5 4 4
    (Playing (whose_turn PlayerTwo))

    To this state:
      0 5 5 0 4 4
    2             0
      0 6 6 6 5 5
    (Playing (whose_turn PlayerOne))
    ("Computer chooses this move" (move 2) (index 2))

    This transitions the game from this state:
      0 5 5 0 4 4
    2             0
      0 6 6 6 5 5
    (Playing (whose_turn PlayerOne))

    To this state:
      1 0 5 0 4 4
    3             0
      1 7 7 6 5 5
    (Playing (whose_turn PlayerTwo))
    ("Computer chooses this move" (move 2) (index 1))

    This transitions the game from this state:
      1 0 5 0 4 4
    3             0
      1 7 7 6 5 5
    (Playing (whose_turn PlayerTwo))

    To this state:
      1 0 5 0 5 5
    3             1
      1 0 8 7 6 6
    (Playing (whose_turn PlayerOne))
  |}]
;;

let%expect_test "The first move of the game is PlayerOne moves square 4, which is optimal" =
  print_computer_move [| 0; 4; 4; 4; 4; 4; 4; 0; 4; 4; 4; 4; 4; 4 |] PlayerOne 1;
  [%expect
    {|
     ("Computer chooses this move" (move 4))

     This transitions the game from this state:
       4 4 4 4 4 4
     0             0
       4 4 4 4 4 4
     (Playing (whose_turn PlayerOne))

     To this state:
       5 5 5 0 4 4
     1             0
       4 4 4 4 4 4
     (Playing (whose_turn PlayerOne))
    |}]
;;

let%expect_test "PlayerOne correctly captures when presented with the opportunity" =
  print_computer_move [| 0; 5; 5; 5; 5; 4; 4; 1; 4; 4; 4; 4; 4; 0 |] PlayerOne 1;
  [%expect
    {|
    ("Computer chooses this move" (move 5))

    This transitions the game from this state:
      0 4 4 4 4 4
    0             1
      5 5 5 5 4 4
    (Playing (whose_turn PlayerOne))

    To this state:
      0 5 5 5 0 4
    6             1
      0 5 5 5 4 4
    (Playing (whose_turn PlayerTwo))
    |}]
;;

(*let%expect_test "O finds an immediate winning move" =
  print_computer_move [ [ E; E; O ]; [ O; X; X ]; [ O; X; O ] ] 1;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 0))))

    This transitions the game from this state:
     | |O
    -----
    O|X|X
    -----
    O|X|O
    (In_progress (whose_turn O))

    To this state:
    O| |O
    -----
    O|X|X
    -----
    O|X|O
    (Winner O)
    |}]
;;

let%expect_test "X prevents an immediate win" =
  print_computer_move [ [ X; E; E ]; [ O; O; E ]; [ X; E; E ] ] 2;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 1) (column 2))))

    This transitions the game from this state:
    X| |
    -----
    O|O|
    -----
    X| |
    (In_progress (whose_turn X))

    To this state:
    X| |
    -----
    O|O|X
    -----
    X| |
    (In_progress (whose_turn O))
    |}]
;;

let%expect_test "O prevents an immediate win" =
  print_computer_move [ [ X; X; E ]; [ O; E; E ]; [ E; E; E ] ] 2;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 2))))

    This transitions the game from this state:
    X|X|
    -----
    O| |
    -----
     | |
    (In_progress (whose_turn O))

    To this state:
    X|X|O
    -----
    O| |
    -----
     | |
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "O prevents another immediate win" =
  print_computer_move [ [ X; O; E ]; [ X; O; E ]; [ E; X; E ] ] 2;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 2) (column 0))))

    This transitions the game from this state:
    X|O|
    -----
    X|O|
    -----
     |X|
    (In_progress (whose_turn O))

    To this state:
    X|O|
    -----
    X|O|
    -----
    O|X|
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "X finds a winning move that will lead to winning in 2 steps" =
  print_computer_move [ [ X; E; E ]; [ O; X; E ]; [ E; E; O ] ] 3;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 1))))

    This transitions the game from this state:
    X| |
    -----
    O|X|
    -----
     | |O
    (In_progress (whose_turn X))

    To this state:
    X|X|
    -----
    O|X|
    -----
     | |O
    (In_progress (whose_turn O))
    |}]
;;

let%expect_test "O finds a winning move that will lead to winning in 2 steps" =
  print_computer_move [ [ E; X; E ]; [ X; X; O ]; [ E; O; E ] ] 3;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 2) (column 2))))

    This transitions the game from this state:
     |X|
    -----
    X|X|O
    -----
     |O|
    (In_progress (whose_turn O))

    To this state:
     |X|
    -----
    X|X|O
    -----
     |O|O
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "O finds a cool winning move that will lead to winning in 2 steps" =
  print_computer_move [ [ X; O; X ]; [ X; E; E ]; [ O; E; E ] ] 3;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 2) (column 1))))

    This transitions the game from this state:
    X|O|X
    -----
    X| |
    -----
    O| |
    (In_progress (whose_turn O))

    To this state:
    X|O|X
    -----
    X| |
    -----
    O|O|
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "O finds the wrong move due to small depth" =
  print_computer_move [ [ X; E; E ]; [ E; E; E ]; [ E; E; E ] ] 3;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 1))))

    This transitions the game from this state:
    X| |
    -----
     | |
    -----
     | |
    (In_progress (whose_turn O))

    To this state:
    X|O|
    -----
     | |
    -----
     | |
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "O finds the correct move when depth is big enough" =
  print_computer_move [ [ X; E; E ]; [ E; E; E ]; [ E; E; E ] ] 6;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 1) (column 1))))

    This transitions the game from this state:
    X| |
    -----
     | |
    -----
     | |
    (In_progress (whose_turn O))

    To this state:
    X| |
    -----
     |O|
    -----
     | |
    (In_progress (whose_turn X))
    |}]
;;

let%expect_test "X finds a winning move that will lead to winning in 2 steps" =
  print_computer_move [ [ E; E; E ]; [ O; X; E ]; [ E; E; E ] ] 5;
  [%expect
    {|
    ("Computer chooses this move" (move ((row 0) (column 0))))

    This transitions the game from this state:
     | |
    -----
    O|X|
    -----
     | |
    (In_progress (whose_turn X))

    To this state:
    X| |
    -----
    O|X|
    -----
     | |
    (In_progress (whose_turn O))
    |}]
;; *)
