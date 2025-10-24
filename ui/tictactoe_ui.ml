open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open! Bonsai.Let_syntax

let app =
  let initial_state =
    Game_state.create ~num_squares_per_side:6 ~init_beads:4
    |> Result.ok
    |> Option.value_exn
  in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state in
  
  (* Simple test: just show the board array as text *)
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.id "board" ]
    [ Vdom.Node.text (sprintf "Board: %s" (Sexp.to_string ([%sexp_of: int array] game_state.board))) ]
;;

let () = Bonsai_web.Start.start app