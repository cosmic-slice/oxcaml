open! Core
open! Stdlib

module Players = struct
  type t =
    | PlayerOne
    | PlayerTwo
  [@@deriving sexp, compare, equal]

  (* It's clearer to use type inference and just write:
     [let opposite t =]
  *)
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
    { player_one_side: int array
    ; player_two_side: int array
    ; player_one_score: int
    ; player_two_score: int
    ; num_squares_per_side: int
    ; init_beads: int
    ; decision : Decision.t
    ; last_move : int option (* For animation purposes. *)
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t =
      | Board_too_big_or_small
    [@@deriving sexp, compare]
  end

  module Move_error = struct
    type t = 
      | Game_is_over
      | Not_a_valid_square
      | Square_is_empty
    [@@deriving sexp, compare]
  end

  let create ~num_squares_per_side ~init_beads : (t, Create_error.t list) Result.t =
    let size_ok = num_squares_per_side < 20 && num_squares_per_side > 0 in
    match size_ok with
    | true ->
      Ok
        { player_one_side = Array.make num_squares_per_side init_beads
        ; player_two_side = Array.make num_squares_per_side init_beads
        ; num_squares_per_side = num_squares_per_side
        ; init_beads = init_beads
        ; player_one_score = 0
        ; player_two_score = 0
        ; decision = Playing { whose_turn = PlayerOne }
        ; last_move = None
        }
    | _ ->
      Error
        ((if size_ok then [] else [ Create_error.Board_too_big_or_small ]))
  ;;

  let is_square_empty t square_index =
    if t.decision = Playing { whose_turn = Players.PlayerOne }
      then t.player_one_side.(square_index) = 0
      else t.player_two_side.(square_index) = 0

  let is_game_over t = Decision.is_game_over t.decision

  let get_current_player t =
    match t.decision with
    | Playing { whose_turn } -> Some whose_turn
    | Winner _ | Tie -> None
  ;;

  (** Checks every position on the board, paired with every one of the eight directions,
      and walks in that direction the length of a winning sequence. If all the cells it
      visits are owned by a player, then that player has won.

      Note that this is not an incredibly efficient algorithm, but it is a simple and
      correct one. One could improve performance and just check all directions around the
      most recently played position (and sum the sequence lengths of opposite directions).

      Also note that if there are multiple win sequences, this algorithm will pick the
      first one it finds. This is fine because game play stops when the first win-sequence
      has been created. *)
  let check_winner t : Decision.t option =
    let player_one_total = Array.fold_left (fun acc x -> acc + x) 0 t.player_one_side in
    let player_two_total = Array.fold_left (fun acc x -> acc + x) 0 t.player_two_side in
    if player_one_total = 0 || player_two_total = 0 then begin
      let final_player_one_score = t.player_one_score + player_one_total in
      let final_player_two_score = t.player_two_score + player_two_total in
      if final_player_one_score > final_player_two_score then Some (Decision.Winner Players.PlayerOne)
      else if final_player_two_score > final_player_one_score then Some (Decision.Winner Players.PlayerTwo)
      else Some Decision.Tie
    end
    else None
  ;;

  let is_valid_square t (square : int) =
    square >= 1 && square <= t.num_squares_per_side
  ;;

  let make_move t (cell_position : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | _ when not (is_legal_cell_position t cell_position) -> Error Illegal_cell_position
    | Winner _ | Stalemate -> Error Game_is_over
    | In_progress { whose_turn } ->
      (match Map.find t.board cell_position with
       | Some _ -> Error Space_already_filled
       | None ->
         let board = Map.set t.board ~key:cell_position ~data:whose_turn in
         let decision : Decision.t =
           match check_winner { t with board } with
           | Some player_kind -> Winner player_kind
           | None ->
             if Map.length board >= t.columns * t.rows
             then Stalemate
             else In_progress { whose_turn = Player_kind.opposite whose_turn }
         in
         Ok { t with board; decision; last_move = Some cell_position })
  ;;

  module For_testing = struct
    let all_directions = all_directions
  end
end
