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
    { player_one_side : int array
    ; player_two_side : int array
    ; player_one_score : int
    ; player_two_score : int
    ; num_squares_per_side : int
    ; init_beads : int
    ; decision : Decision.t
    ; last_move : int option (* For animation purposes. *)
    }
  [@@deriving sexp, compare, equal]

  module Create_error = struct
    type t = Board_too_big_or_small [@@deriving sexp, compare]
  end

  module Move_error = struct
    type t =
      | Game_is_over
      | Not_a_valid_square
      | Square_is_empty
    [@@deriving sexp, compare]
  end

  (* Create an instance of the board module with a set number of squares per 
     side and an initial number of beads *)
  let create ~num_squares_per_side ~init_beads : (t, Create_error.t list) Result.t =
    let size_ok = num_squares_per_side < 20 && num_squares_per_side > 0 in
    match size_ok with
    | true ->
      Ok
        { player_one_side = Array.make num_squares_per_side init_beads
        ; player_two_side = Array.make num_squares_per_side init_beads
        ; num_squares_per_side
        ; init_beads
        ; player_one_score = 0
        ; player_two_score = 0
        ; decision = Playing { whose_turn = PlayerOne }
        ; last_move = None
        }
    | _ -> Error (if size_ok then [] else [ Create_error.Board_too_big_or_small ])
  ;;

  (* Check if a square is empty. The square that's checked belongs to the current
     player*)
  let is_square_empty t square_index : bool =
    if t.decision = Playing { whose_turn = Players.PlayerOne }
    then t.player_one_side.(square_index) = 0
    else t.player_two_side.(square_index) = 0
  ;;

  let is_game_over t = Decision.is_game_over t.decision

  let get_current_player t =
    match t.decision with
    | Playing { whose_turn } -> Some whose_turn
    | Winner _ | Tie -> None
  ;;

  (** Determine if any side of the board is empty and then compare scores
      to check for a winner *)
  let check_winner t : t option =
    let player_one_total = Array.fold_left (fun acc x -> acc + x) 0 t.player_one_side in
    let player_two_total = Array.fold_left (fun acc x -> acc + x) 0 t.player_two_side in
    if player_one_total = 0 || player_two_total = 0
    then (
      let final_player_one_score = t.player_one_score + player_one_total in
      let final_player_two_score = t.player_two_score + player_two_total in
      (* Clear each side of the board *)
      Array.fill t.player_one_side 0 (Array.length t.player_one_side) 0;
      Array.fill t.player_two_side 0 (Array.length t.player_two_side) 0;
      (* Determine winner and return new state *)
      let final_decision =
        if final_player_one_score > final_player_two_score
        then Decision.Winner Players.PlayerOne
        else if final_player_two_score > final_player_one_score
        then Decision.Winner Players.PlayerTwo
        else Decision.Tie
      in
      Some
        { t with
          player_one_score = final_player_one_score
        ; player_two_score = final_player_two_score
        ; decision = final_decision
        })
    else None
  ;;

  (* Make sure the user has picked a valid square number. This is useful for testing,
     but is unnecessary to check with the UI in place *)
  let is_valid_square t (square : int) = square >= 0 && square < t.num_squares_per_side

  let rec distribute_beads t (side : Players.t) (index : int) (beads_remaining : int) =
    if beads_remaining = 1
    then (
      match t.decision with
      | Playing { whose_turn } ->
        if whose_turn = Players.PlayerOne && index < 0
        then (
          (* If PlayerOne has ended on his goal, he gets an extra turn *)
          let t' =
            { t with
              player_one_score = t.player_one_score + 1
            ; decision = Playing { whose_turn = Players.PlayerOne }
            }
          in
          Ok t')
        else if whose_turn = Players.PlayerTwo && index > t.num_squares_per_side - 1
        then (
          (* If PlayerTwo has ended on his goal, he gets an extra turn *)
          let t' =
            { t with
              player_two_score = t.player_two_score + 1
            ; decision = Playing { whose_turn = Players.PlayerTwo }
            }
          in
          Ok t')
        else if
          whose_turn = side
          && ((whose_turn = Players.PlayerTwo && t.player_two_side.(index) = 0)
              || (whose_turn = Players.PlayerOne && t.player_one_side.(index) = 0))
        then (
          (* If a player ends on his side AND on an empty tile, he steals the opponent's beads *)
          let beads_to_steal =
            t.player_one_side.(index) + t.player_two_side.(index) + 1
          in
          let t' =
            if whose_turn = Players.PlayerOne
            then (
              t.player_one_side.(index) <- 0;
              t.player_two_side.(index) <- 0;
              { t with
                player_one_score = t.player_one_score + beads_to_steal
              ; decision = Playing { whose_turn = Players.PlayerTwo }
              })
            else (
              t.player_one_side.(index) <- 0;
              t.player_two_side.(index) <- 0;
              { t with
                player_two_score = t.player_two_score + beads_to_steal
              ; decision = Playing { whose_turn = Players.PlayerOne }
              })
          in
          Ok t')
        else (
          (* If no special case is triggered, just increment bead count in current index *)
          let next_turn = Players.opposite whose_turn in
          if side = Players.PlayerOne
          then t.player_one_side.(index) <- t.player_one_side.(index) + 1
          else t.player_two_side.(index) <- t.player_two_side.(index) + 1;
          Ok { t with decision = Playing { whose_turn = next_turn } })
      | Tie | Winner _ -> Error Move_error.Game_is_over)
    else (
      match t.decision with
      | Playing _ ->
        (* If on PlayerOne's side, go around the board counter-clockwise by decrementing
           the index *)
        if side = Players.PlayerOne
        then
          if
            (* If index < 0 (i.e. goal reached) increment and loop to other side *)
            index < 0
          then (
            let t' = { t with player_one_score = t.player_one_score + 1 } in
            distribute_beads t' Players.PlayerTwo 0 (beads_remaining - 1))
          else (
            t.player_one_side.(index) <- t.player_one_side.(index) + 1;
            distribute_beads t side (index - 1) (beads_remaining - 1))
        else if
          (* If on PlayerTwo's side, go around the board clockwise by incrementing
           the index *)
          index > t.num_squares_per_side - 1
        then (
          let t' = { t with player_two_score = t.player_two_score + 1 } in
          distribute_beads
            t'
            Players.PlayerOne
            (t.num_squares_per_side - 1)
            (beads_remaining - 1))
        else (
          t.player_two_side.(index) <- t.player_two_side.(index) + 1;
          distribute_beads t side (index + 1) (beads_remaining - 1))
      | _ -> Error Move_error.Game_is_over)
  ;;

  (* Make a move on the board. The moves are represented as values from 1 to
     num_squares_per_side and correspond to the squares on each player's side *)
  let make_move t (move : int) : (t, Move_error.t) Result.t =
    match t.decision with
    | _ when not (is_valid_square t move) -> Error Move_error.Not_a_valid_square
    | Winner _ | Tie -> Error Game_is_over
    | Playing { whose_turn } ->
      (* If playing, get the number of beads at the specified index on the player's side*)
      let num_beads =
        match whose_turn with
        | Players.PlayerOne -> t.player_one_side.(move)
        | Players.PlayerTwo -> t.player_two_side.(move)
      in
      (* If the square is empty, return Square_is_empty error *)
      if num_beads = 0
      then Error Move_error.Square_is_empty
      else (
        (* Take out all beads from selected square and distribute beads over board *)
        match whose_turn with
        | Players.PlayerOne ->
          t.player_one_side.(move) <- 0;
          (match distribute_beads t whose_turn (move - 1) num_beads with
           | Ok t' ->
             let t'' = { t' with last_move = Some move } in
             (* Check if the game is over *)
             (match check_winner t'' with
              | Some final_state -> Ok final_state
              | None -> Ok t'')
           | Error e -> Error e)
        | Players.PlayerTwo ->
          t.player_two_side.(move) <- 0;
          (match distribute_beads t whose_turn (move + 1) num_beads with
           | Ok t' ->
             let t'' = { t' with last_move = Some move } in
             (* Check if the game is over *)
             (match check_winner t'' with
              | Some final_state -> Ok final_state
              | None -> Ok t'')
           | Error e -> Error e))
  ;;
end
