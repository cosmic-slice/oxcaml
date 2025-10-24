open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open! Bonsai.Let_syntax

let svg_ns = "http://www.w3.org/2000/svg"
let viewbox = Vdom.Attr.create "viewBox" "0 0 100 100"

let colors = [| "red"; "blue"; "green"; "yellow" |]
let bead_radius = 10.0
let goal_bead_radius = 5.0
let ring_multiplier = 6

(* IDs map to board positions in circular order *)
let ids = 
  [| "p1_goal"; "p2_1"; "p2_2"; "p2_3"; "p2_4"; "p2_5"; "p2_6"; 
     "p2_goal"; "p1_6"; "p1_5"; "p1_4"; "p1_3"; "p1_2"; "p1_1" |]

let create_bead ~cx ~cy ~radius ~color =
  Vdom.Node.create_svg
    "circle"
    ~attrs:
      [ Vdom.Attr.create "cx" (Float.to_string cx ^ "%")
      ; Vdom.Attr.create "cy" (Float.to_string cy ^ "%")
      ; Vdom.Attr.create "r" (Float.to_string radius ^ "%")
      ; Vdom.Attr.create "fill" color
      ]
    []
;;

let render_beads ~num_beads ~is_goal ~pit_index =
  let cx = 50.0 in
  let cy = 50.0 in
  let radius = if is_goal then goal_bead_radius else bead_radius in
  
  let beads = ref [] in
  let distribution_radius = ref 0.0 in
  let ring = ref 0 in
  let index_in_ring = ref 0 in
  
  for j = 0 to num_beads - 1 do
    let dx, dy =
      if j = 0 then
        0.0, 0.0
      else
        let num_beads_in_ring = 
          Int.min 
            (Int.pow ring_multiplier !ring - 1) 
            (num_beads - Int.pow ring_multiplier (!ring - 1))
        in
        let angle = 
          Float.of_int (!index_in_ring - 1) *. (2.0 *. Float.pi /. Float.of_int num_beads_in_ring)
        in
        !distribution_radius *. Float.cos angle, !distribution_radius *. Float.sin angle
    in
    
    let color = colors.((pit_index + j) % 4) in
    let bead = create_bead ~cx:(cx +. dx) ~cy:(cy +. dy) ~radius ~color in
    beads := bead :: !beads;
    
    index_in_ring := !index_in_ring + 1;
    if !index_in_ring = Int.pow ring_multiplier !ring then (
      distribution_radius := !distribution_radius +. (if is_goal then 15.0 else 20.0);
      index_in_ring := 0;
      ring := !ring + 1
    )
  done;
  
  Vdom.Node.create_svg
    "svg"
    ~attrs:
      [ Vdom.Attr.create "width" "100%"
      ; Vdom.Attr.create "height" "100%"
      ]
    (List.rev !beads)
;;

let mancala_board ~(game_state : Game_state.t) ~set_game_state =
  let is_game_over = Decision.is_game_over game_state.decision in
  
  let render_pit ~pit_index ~is_goal ~player_class =
    let num_beads = game_state.board.(pit_index) in
    let id_class = ids.(pit_index) in
    let beads_svg = render_beads ~num_beads ~is_goal ~pit_index in
    
    let maybe_clickable_attr =
      if is_game_over || is_goal then
        Vdom.Attr.empty
      else
        Vdom.Attr.on_click (fun _ ->
          match Game_state.make_move game_state pit_index with
          | Error _ -> raise_s [%message "Invalid move" (pit_index : int)]
          | Ok new_game_state -> set_game_state new_game_state)
    in
    
    let class_list = 
      (if is_goal then "goal" else "pit") :: 
      (if String.is_empty player_class then [] else [player_class])
    in
    
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.id id_class
        ; Vdom.Attr.classes class_list
        ; maybe_clickable_attr
        ]
      [ beads_svg ]
  in
  
  let top_row =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.id "top_row" ]
      (List.init 6 ~f:(fun i -> render_pit ~pit_index:(13 - i) ~is_goal:false ~player_class:"p1"))
  in
  
  let bottom_row =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.id "bottom_row" ]
      (List.init 6 ~f:(fun i -> render_pit ~pit_index:(i + 1) ~is_goal:false ~player_class:"p2"))
  in
  
  let middle_rows =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.id "middle_rows" ]
      [ top_row; bottom_row ]
  in
  
  let p1_goal = render_pit ~pit_index:0 ~is_goal:true ~player_class:"" in
  let p2_goal = render_pit ~pit_index:7 ~is_goal:true ~player_class:"" in
  
  let board =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.id "board" ]
      [ p1_goal; middle_rows; p2_goal ]
  in
  
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.class_ "game" ]
    [ board ]
;;

let app =
  let initial_state = Game_state.create ~num_squares_per_side:6 ~init_beads:4 
    |> Result.ok
    |> Option.value_exn
  in
  let%sub game_state, set_game_state =
    Bonsai.state ~default_model:initial_state (module Game_state)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state in
  mancala_board ~game_state ~set_game_state
;;

let () = Bonsai_web.Start.start app