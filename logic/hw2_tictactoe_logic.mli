open! Core

module Players : sig
  type t =
    | PlayerOne
    | PlayerTwo
  [@@deriving sexp, compare, equal]

  val opposite : t -> t
end

module Decision : sig
  type t =
    | Playing of { whose_turn : Players.t }
    | Winner of Players.t
    | Tie
  [@@deriving sexp, compare, equal]

  val is_game_over : t -> bool
end

module Game_state : sig
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

  module Create_error : sig
    type t = Board_too_big_or_small [@@deriving sexp, compare]
  end

  module Move_error : sig
    type t =
      | Game_is_over
      | Not_a_valid_square
      | Square_is_empty
    [@@deriving sexp, compare]
  end

  val create
    :  num_squares_per_side:int
    -> init_beads:int
    -> (t, Create_error.t list) Result.t

  val is_square_empty : t -> int -> bool
  val is_game_over : t -> bool
  val get_current_player : t -> Players.t option
  val check_winner : t -> t option
  val is_valid_square : t -> int -> bool
  val distribute_beads : t -> Players.t -> int -> int -> (t, Move_error.t) result
  val make_move : t -> int -> (t, Move_error.t) Result.t
end
