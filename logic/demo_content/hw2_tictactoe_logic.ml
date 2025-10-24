open! Core
open! Stdlib

(* This is an implementation of Mancala in OCaml. The board is designed as an
   array that the players loop around. The number of initial beads is variable
   and so is the number of squares on each side. PlayerOne's goal is located
   at index 0 and PlayerTwo's goal is at num_squares_per_side + 1 *)
module Players = struct
  type t =
    | PlayerOne
    | PlayerTwo
  [@@deriving sexp, compare, equal]

  (* Get opposite player using matching *)
  let opposite (t : t) : t =
    match t with
    | PlayerOne -> PlayerTwo
    | PlayerTwo -> PlayerOne
  ;;
end

module Decision = struct
  type t =
    | Playing of { whose_turn : Players.t }
    | Winner of Players.t
    | Tie
  [@@deriving sexp, compare, equal]

  let is_game_over t =
    match t with
    | Tie | Winner _ -> true
    | Playing _ -> false
  ;;
end

module Game_state = struct
  type t =
    { board : int array
    ; num_squares_per_side : int
    ; decision : Decision.t
    ; last_move : Players.t option * int option
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t =
      | Board_too_big_or_small
      | Bead_count_invalid
    [@@deriving sexp, compare]
  end

  module Move_error = struct
    type t =
      | Game_is_over
      | Not_a_valid_square
      | Square_is_empty
    [@@deriving sexp, compare]
  end

  (* Method for generating a new board with set number of squares, and initial beads*)
  let get_init_board num_squares_per_side init_beads =
    let board = Array.make ((2 * num_squares_per_side) + 2) init_beads in
    board.(0) <- 0;
    (* PlayerOne goal *)
    board.(num_squares_per_side + 1) <- 0;
    (* PlayerTwo goal *)
    board
  ;;

  let get_goal_index t (player : Players.t) =
    match player with
    | PlayerOne -> 0
    | PlayerTwo -> t.num_squares_per_side + 1
  ;;

  (* Create an instance of the board module with a set number of squares per 
     side and an initial number of beads *)
  let create ~num_squares_per_side ~init_beads : (t, Create_error.t list) Result.t =
    let size_ok = num_squares_per_side >= 6 && num_squares_per_side <= 12 in
    let num_beads_ok = init_beads >= 4 && init_beads <= 10 in
    match size_ok, num_beads_ok with
    | true, true ->
      Ok
        { board = get_init_board num_squares_per_side init_beads
        ; num_squares_per_side
        ; decision = Playing { whose_turn = Players.PlayerOne }
        ; last_move = None, None
        }
    | _ ->
      Error
        ((if size_ok then [] else [ Create_error.Board_too_big_or_small ])
         @ if num_beads_ok then [] else [ Create_error.Bead_count_invalid ])
  ;;

  (* Check if a specified index is a goal index or not*)
  let is_not_goal t square_index =
    square_index != get_goal_index t Players.PlayerOne
    && square_index != get_goal_index t Players.PlayerTwo
  ;;

  (* Check if a square is empty. The square that's checked belongs to the current
     player*)
  let is_square_empty t square_index = t.board.(square_index) = 0

  (* Check if the game is over *)
  let is_game_over t = Decision.is_game_over t.decision

  (* Get the score of the player passed to the method *)
  let get_score t (player : Players.t) =
    match player with
    | Players.PlayerOne -> t.board.(get_goal_index t Players.PlayerOne)
    | Players.PlayerTwo -> t.board.(get_goal_index t Players.PlayerTwo)
  ;;

  (* Set score for player passed to the method *)
  let change_score t (player : Players.t) amount =
    match player with
    | Players.PlayerOne ->
      t.board.(get_goal_index t Players.PlayerOne)
      <- get_score t Players.PlayerOne + amount
    | Players.PlayerTwo ->
      t.board.(get_goal_index t Players.PlayerTwo)
      <- get_score t Players.PlayerTwo + amount
  ;;

  (* Figure out who the current player is *)
  let get_current_player t =
    match t.decision with
    | Playing { whose_turn } -> whose_turn
    | Winner _ | Tie -> failwith "Game is over"
  ;;

  (* Determine if any side of the board is empty and then compare scores
      to check for a winner *)
  let do_gameover t : t =
    (* Slice the board array to get each player's side*)
    let player_one_side =
      Array.sub t.board (get_goal_index t Players.PlayerTwo + 1) t.num_squares_per_side
    in
    let player_two_side =
      Array.sub t.board (get_goal_index t Players.PlayerOne + 1) t.num_squares_per_side
    in
    (* Fold each side to get the total number of beads *)
    let player_one_total = Array.fold_left (fun acc x -> acc + x) 0 player_one_side in
    let player_two_total = Array.fold_left (fun acc x -> acc + x) 0 player_two_side in
    (* If either side is empty, the game is over*)
    if player_one_total = 0 || player_two_total = 0
    then (
      (* Move all of beads on each player's side to their goal *)
      change_score t Players.PlayerOne player_one_total;
      change_score t Players.PlayerTwo player_two_total;
      (* Clear each side of the board *)
      Array.fill t.board (get_goal_index t Players.PlayerOne + 1) t.num_squares_per_side 0;
      Array.fill t.board (get_goal_index t Players.PlayerTwo + 1) t.num_squares_per_side 0;
      (* Determine winner and return new state *)
      let final_decision =
        if get_score t Players.PlayerOne > get_score t Players.PlayerTwo
        then Decision.Winner Players.PlayerOne
        else if get_score t Players.PlayerTwo > get_score t Players.PlayerOne
        then Decision.Winner Players.PlayerTwo
        else Decision.Tie
      in
      { t with decision = final_decision })
    else t
  ;;

  (* Make sure the user has picked a valid square number. This is useful for testing,
     but is unnecessary to check with the UI in place *)
  let is_valid_move t (square : int) = square >= 1 && square <= t.num_squares_per_side

  (* Determine whether the index provided is the opposite player's goal *)
  let is_opposite_players_goal t index =
    let current_player = get_current_player t in
    match current_player with
    | Players.PlayerOne -> index = get_goal_index t Players.PlayerTwo
    | Players.PlayerTwo -> index = get_goal_index t Players.PlayerOne
  ;;

  (* Determine whether the index provided is the player's goal *)
  let is_players_goal t index =
    let current_player = get_current_player t in
    match current_player with
    | Players.PlayerOne -> index = get_goal_index t Players.PlayerOne
    | Players.PlayerTwo -> index = get_goal_index t Players.PlayerTwo
  ;;

  (* Determine whether an index is on the player's side or not *)
  let on_players_side t index current_player =
    match current_player with
    | Players.PlayerOne -> index > get_goal_index t Players.PlayerTwo
    | Players.PlayerTwo ->
      index > get_goal_index t Players.PlayerOne
      && index < get_goal_index t Players.PlayerTwo
  ;;

  (* Steal all beads from current square and the one opposite it *)
  let do_steal t index current_player =
    if on_players_side t index current_player
    then (
      let opposite_index = Array.length t.board - index in
      if t.board.(opposite_index) > 0
      then (
        let beads_stolen = t.board.(index) + t.board.(opposite_index) in
        t.board.(index) <- 0;
        t.board.(opposite_index) <- 0;
        change_score t current_player beads_stolen))
  ;;

  (* Recursively distribute the beads around the board*)
  let rec distribute_beads t (index : int) (beads_remaining : int) =
    match t.decision with
    | Playing _ ->
      if beads_remaining = 0
      then (
        (* Determine who the current player is *)
        let current_player = get_current_player t in
        (* If the player landed in his own goal, he gets an extra turn *)
        if is_players_goal t index
        then Ok { t with decision = Playing { whose_turn = current_player } }
        else (
          (* If the last bead was placed on an empty square, do steal*)
          if t.board.(index) = 1 then do_steal t index current_player;
          (* Change turn to other player's side *)
          Ok
            { t with decision = Playing { whose_turn = Players.opposite current_player } }))
      else (
        (* Calculate the next index to add beads to *)
        let next_index = (index + 1) mod Array.length t.board in
        (* If the player is over the other player's goal, skip it *)
        if is_opposite_players_goal t next_index
        then distribute_beads t next_index beads_remaining
        else (
          (* Move a bead over to the next square *)
          t.board.(next_index) <- t.board.(next_index) + 1;
          distribute_beads t next_index (beads_remaining - 1)))
    | Tie | Winner _ -> Error Move_error.Game_is_over
  ;;

  (* Get a list of all possible moves *)
  let get_all_moves t = List.init t.num_squares_per_side (fun i -> i + 1)

  (* Make a move on the board. The moves are represented as values from 1 to
     num_squares_per_side and correspond to the squares on each player's side *)
  let make_move t (move : int) : (t, Move_error.t) Result.t =
    match t.decision with
    | _ when not (is_valid_move t move) -> Error Move_error.Not_a_valid_square
    | Winner _ | Tie -> Error Game_is_over
    | Playing { whose_turn } ->
      (* If playing, get the number of beads at the specified index on the player's side*)
      let adjusted_move =
        match whose_turn with
        | Players.PlayerOne -> Array.length t.board - move
        | Players.PlayerTwo -> move
      in
      let t =
        { t with board = Array.copy t.board; last_move = Some whose_turn, Some move }
      in
      let num_beads = t.board.(adjusted_move) in
      if is_square_empty t adjusted_move
      then Error Move_error.Square_is_empty
      else (
        t.board.(adjusted_move) <- 0;
        let t' = distribute_beads t adjusted_move num_beads in
        match t' with
        | Ok t' -> Ok (do_gameover t')
        | Error err -> Error err)
  ;;
end
