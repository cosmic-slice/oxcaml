open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open! Bonsai.Let_syntax

(* --- Constants --- *)

(* For the beads, we use the user coordinate system (0-100) inside the SVG viewbox.
   We need to append "%" to the attributes to place them relative to the pit's SVG area. *)
let viewbox = Vdom.Attr.create "viewBox" "0 0 100 100"

let colors = [| "red"; "blue"; "green"; "yellow" |]
let bead_radius = "10%" (* Use float for radius calculation *)
let goal_bead_radius = "5%"
let ring_multiplier = 6

(* IDs map to board positions in circular order, same as your JS/HTML *)
let ids = 
  [| "p1_goal"; "p2_1"; "p2_2"; "p2_3"; "p2_4"; "p2_5"; "p2_6"; 
     "p2_goal"; "p1_6"; "p1_5"; "p1_4"; "p1_3"; "p1_2"; "p1_1" |]

(* --- SVG Rendering Functions --- *)

let create_bead ~cx ~cy ~radius ~color =
  let radius_str = sprintf "%.1f%%" radius in (* Use "10%" and "5%" as in JS *)
  Vdom.Node.create_svg
    "circle"
    ~attrs:
      [ Vdom.Attr.create "cx" (sprintf "%.1f%%" cx) (* Append % for placement relative to pit *)
      ; Vdom.Attr.create "cy" (sprintf "%.1f%%" cy) (* Append % for placement relative to pit *)
      ; Vdom.Attr.create "r" radius_str
      ; Vdom.Attr.create "fill" color
      ]
    []
;;

let render_beads ~num_beads ~is_goal ~pit_index =
  let cx = 50.0 in (* Center X *)
  let cy = 50.0 in (* Center Y *)
  let radius = if is_goal then goal_bead_radius else bead_radius in
  
  (* Recursive function to distribute beads *)
  let rec distribute_beads j ring index_in_ring distribution_radius acc =
    if j >= num_beads then List.rev acc
    else
      let dx, dy, next_dist, next_ring, next_index =
        if j = 0 then (* First bead is at the center *)
          0.0, 0.0, 0.0, 1, 0
        else
          let ring_dist = if is_goal then 15.0 else 20.0 in
          let prev_ring_count = Int.pow ring_multiplier (ring - 1) in
          let current_ring_count = Int.pow ring_multiplier ring in

          (* Beads in the current ring, or remaining beads if fewer than a full ring *)
          let num_beads_in_ring = 
            if j < current_ring_count then 
              j - prev_ring_count + 1 
            else 
              current_ring_count - prev_ring_count
          in
          
          let angle = if num_beads_in_ring = 0 then 0.0 else
            Float.of_int (index_in_ring) *. 
            (2.0 *. Float.pi /. Float.of_int num_beads_in_ring)
          in
          
          let dx = distribution_radius *. Float.cos angle in
          let dy = distribution_radius *. Float.sin angle in
          
          let next_index = index_in_ring + 1 in
          if next_index >= num_beads_in_ring then
            (dx, dy, distribution_radius +. ring_dist, ring + 1, 0)
          else
            (dx, dy, distribution_radius, ring, next_index)
      in
      
      let color = colors.((pit_index + j) % 4) in
      let bead = create_bead ~cx:(cx +. dx) ~cy:(cy +. dy) ~radius ~color in
      
      distribute_beads (j + 1) next_ring next_index next_dist (bead :: acc)
  in
  
  let beads = distribute_beads 0 1 0 20.0 [] in (* Initial ring is 1, radius 20 *)
  
  Vdom.Node.create_svg
    "svg"
    ~attrs:
      [ Vdom.Attr.create "width" "100%"
      ; Vdom.Attr.create "height" "100%"
      ; viewbox
      ]
    beads
;;

(* --- Mancala Board Component --- *)

let mancala_board ~(game_state : Game_state.t) ~set_game_state =
  let is_game_over = Game_state.is_game_over game_state in
  
  let render_pit ~board_index ~is_goal ~player_class ~move_number =
    let num_beads = game_state.board.(board_index) in
    let id_class = ids.(board_index) in
    let beads_svg = render_beads ~num_beads ~is_goal ~pit_index:board_index in
    
    let maybe_clickable_attr =
      if is_game_over || is_goal || Option.is_none move_number then
        Vdom.Attr.empty
      else
        let move = Option.value_exn move_number in
        Vdom.Attr.on_click (fun _ ->
          match Game_state.make_move game_state move with
          | Error _ -> raise_s [%message "Invalid move" (move : int)]
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
  
  (* Top row: Player One's side (right to left: indices 13..8 -> moves 1..6) *)
  let top_row =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.id "top_row" ]
      (List.init game_state.num_squares_per_side ~f:(fun i -> 
        let board_index = Array.length game_state.board - 1 - i in
        let move = i + 1 in
        render_pit ~board_index ~is_goal:false ~player_class:"p1" ~move_number:(Some move)))
  in
  
  (* Bottom row: Player Two's side (left to right: indices 1..6 -> moves 1..6) *)
  let bottom_row =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.id "bottom_row" ]
      (List.init game_state.num_squares_per_side ~f:(fun i -> 
        let board_index = i + 1 in
        let move = i + 1 in
        render_pit ~board_index ~is_goal:false ~player_class:"p2" ~move_number:(Some move)))
  in
  
  let middle_rows =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.id "middle_rows" ]
      [ top_row; bottom_row ]
  in
  
  let p1_goal = render_pit ~board_index:0 ~is_goal:true ~player_class:"" ~move_number:None in
  let p2_goal = render_pit ~board_index:(game_state.num_squares_per_side + 1) ~is_goal:true ~player_class:"" ~move_number:None in
  
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
  mancala_board ~game_state ~set_game_state
;;

let () = Bonsai_web.Start.start app