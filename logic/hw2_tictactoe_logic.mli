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
    { board : int array
    ; num_squares_per_side : int
    ; decision : Decision.t
    ; last_move : int option (* For animation purposes. *)
    }
  [@@deriving sexp, compare, equal]

  module Create_error : sig
    type t =
      | Board_too_big_or_small
      | Bead_count_invalid
    [@@deriving sexp, compare]
  end

  module Move_error : sig
    type t =
      | Game_is_over
      | Not_a_valid_square
      | Square_is_empty
    [@@deriving sexp, compare]
  end

  val get_init_board : int -> int -> int array
  val get_goal_index : t -> Players.t -> int

  val create
    :  num_squares_per_side:int
    -> init_beads:int
    -> (t, Create_error.t list) Result.t

  val is_not_goal : t -> int -> bool
  val is_square_empty : t -> int -> bool
  val is_game_over : t -> bool
  val get_score : t -> Players.t -> int
  val change_score : t -> Players.t -> int -> unit
  val get_current_player : t -> Players.t
  val do_gameover : t -> t
  val is_valid_move : t -> int -> bool
  val is_opposite_players_goal : t -> int -> bool
  val is_players_goal : t -> int -> bool
  val on_players_side : t -> int -> Players.t -> bool
  val do_steal : t -> int -> Players.t -> unit
  val distribute_beads : t -> int -> int -> (t, Move_error.t) result
  val get_all_moves : t -> int list
  val make_move : t -> int -> (t, Move_error.t) Result.t
end
