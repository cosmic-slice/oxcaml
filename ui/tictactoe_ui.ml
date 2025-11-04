
open! Core
open Tictactoe_logic_library
open Hw2_tictactoe_logic
open Virtual_dom
open! Bonsai.Let_syntax

(* All of the bead constants and SVG viewbox are declared here *)

let viewbox = Vdom.Attr.create "viewBox" "0 0 100 100"
let colors = [| "red"; "blue"; "green"; "yellow" |]
let bead_radius = 1.0 (* vh units *)
let distribution_radius = 25.0
let center_threshold = 6
let cx = 50.0
let cy = 50.0

(* IDs map to board positions in circular order *)
let ids =
  [| "p1_goal"; "p2_1"; "p2_2"; "p2_3"; "p2_4"; "p2_5"; "p2_6"
    ; "p2_goal"; "p1_6"; "p1_5"; "p1_4"; "p1_3"; "p1_2"; "p1_1"
  |]
;;

(* --- SVG Rendering Functions --- *)

let create_bead_at ~x ~y ~radius ~color =
  (* Draw shadow circle *)
  let shadow =
    Vdom.Node.create_svg
      "circle"
      ~attrs:
        [ Vdom.Attr.create "cx" (sprintf "%.1f%%" x)
        ; Vdom.Attr.create "cy" (sprintf "calc(%.1f%% + 1.5px)" y)
        ; Vdom.Attr.create "r" (sprintf "%.1fvh" radius)
        ; Vdom.Attr.create "fill" "black"
        ; Vdom.Attr.create "opacity" "0.5"
        ]
      []
  in
  (* Draw base bead circle *)
  let bead =
    Vdom.Node.create_svg
      "circle"
      ~attrs:
        [ Vdom.Attr.create "cx" (sprintf "%.1f%%" x)
        ; Vdom.Attr.create "cy" (sprintf "%.1f%%" y)
        ; Vdom.Attr.create "r" (sprintf "%.1fvh" radius)
        ; Vdom.Attr.create "fill" color
        ]
      []
  in
  (* Draw highlight circle *)
  let highlight =
    Vdom.Node.create_svg
      "circle"
      ~attrs:
        [ Vdom.Attr.create "cx" (sprintf "calc(%.1f%% + 0.33vh)" x)
        ; Vdom.Attr.create "cy" (sprintf "calc(%.1f%% - 0.33vh)" y)
        ; Vdom.Attr.create "r" (sprintf "%.1fvh" (radius *. 0.33))
        ; Vdom.Attr.create "fill" "white"
        ; Vdom.Attr.create "opacity" "0.5"
        ]
      []
  in
  [ shadow; bead; highlight ]
;;

let render_beads ~num_beads ~pit_index =
  let rec create_beads j acc =
    if j >= num_beads
    then List.rev acc
    else (
      let j_float = Float.of_int j in
      let pit_index_float = Float.of_int pit_index in
      
      (* Determine whether the first bead should be centered or not *)
      let is_center_bead = num_beads = 1 || num_beads >= center_threshold in
      
      let dx, dy =
        if j = 0 && is_center_bead
        then 0.0, 0.0
        else (
          (* Calculate next direction and position differential *)
          let num_distributed = if is_center_bead then num_beads - 1 else num_beads in
          let angle = 
            (j_float *. (2.0 *. Float.pi) /. float_of_int num_distributed) 
            +. pit_index_float 
          in
          let dx = distribution_radius *. Float.cos angle in
          let dy = distribution_radius *. Float.sin angle in
          dx, dy)
      in
      
      (* Prepare values for creating bead *)
      let x = cx +. dx in
      let y = cy +. dy in
      let color = colors.((pit_index + j) % 4) in
      let bead = create_bead_at ~x ~y ~radius:bead_radius ~color in
      create_beads (j + 1) (bead :: acc))
  in
  
  let beads = create_beads 0 [] in
  Vdom.Node.create_svg
    "svg"
    ~attrs:
      [ Vdom.Attr.create "width" "100%"
      ; Vdom.Attr.create "height" "100%"
      ]
    (List.concat beads)
;;

let mancala_board ~(game_state : Game_state.t) ~set_game_state =
  let is_game_over = Game_state.is_game_over game_state in
  let board = game_state.board in
  
  let render_pit ~board_index ~is_goal ~player_class ~move_number =
    let num_beads = board.(board_index) in
    let id_class = ids.(board_index) in
    let beads_svg = render_beads ~num_beads ~pit_index:board_index in
    
    let belongs_to_player =
      match player_class, game_state.decision with
      | "p1", Playing { whose_turn = Players.PlayerOne } -> true
      | "p2", Playing { whose_turn = Players.PlayerTwo } -> true
      | _ -> false
    in
    
    let maybe_clickable_attr =
      if (not belongs_to_player) || is_game_over || is_goal || Option.is_none move_number
      then Vdom.Attr.empty
      else (
        let move = Option.value_exn move_number in
        Vdom.Attr.on_click (fun _ ->
          match Game_state.make_move game_state move with
          | Error _ -> raise_s [%message "Invalid move" (move : int)]
          | Ok new_game_state -> set_game_state new_game_state))
    in
    
    let class_list =
      (if is_goal then "goal" else "pit")
      :: (if String.is_empty player_class then [] else [ player_class ])
    in
    
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.id id_class; Vdom.Attr.classes class_list; maybe_clickable_attr ]
      [ beads_svg ]
  in
  
  (* HUD Elements *)
  let player1_score =
    Vdom.Node.create
      "p"
      ~attrs:[ Vdom.Attr.id "player1_score" ]
      [ Vdom.Node.create ~attrs:[] "strong" [ Vdom.Node.text "PLAYER 1" ]
      ; Vdom.Node.create ~attrs:[] "br" []
      ; Vdom.Node.text (sprintf "Score: %d" (Game_state.get_score game_state Players.PlayerOne))
      ]
  in
  
  let player2_score =
    Vdom.Node.create
      "p"
      ~attrs:[ Vdom.Attr.id "player2_score" ]
      [ Vdom.Node.create ~attrs:[] "strong" [ Vdom.Node.text "PLAYER 2" ]
      ; Vdom.Node.create ~attrs:[] "br" []
      ; Vdom.Node.text (sprintf "Score: %d" (Game_state.get_score game_state Players.PlayerTwo))
      ]
  in
  
  let game_status =
    let status_text =
      match game_state.decision with
      | Playing { whose_turn = Players.PlayerOne } -> "P1's Turn!"
      | Playing { whose_turn = Players.PlayerTwo } -> "P2's Turn!"
      | Winner Players.PlayerOne -> "P1 Wins!"
      | Winner Players.PlayerTwo -> "P2 Wins!"
      | Tie -> "It's a Tie!"
    in
    Vdom.Node.create "p" ~attrs:[ Vdom.Attr.id "game_status" ] [ Vdom.Node.text status_text ]
  in
  
  let hud =
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.class_ "hud" ]
      [ Vdom.Node.create "div" ~attrs:[ Vdom.Attr.class_ "score" ] [ player1_score ]
      ; game_status
      ; Vdom.Node.create "div" ~attrs:[ Vdom.Attr.class_ "score" ] [ player2_score ]
      ]
  in
  
  (* Top numbers *)
  let top_numbers =
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.id "top_numbers" ]
      (List.init (Array.length board / 2 - 1) ~f:(fun i ->
         let board_index = Array.length board - 1 - i in
         Vdom.Node.create ~attrs:[] "p" [ Vdom.Node.text (Int.to_string board.(board_index)) ]))
  in
  
  (* Top row: Player One's pits *)
  let top_row =
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.id "top_row" ]
      (List.init game_state.num_squares_per_side ~f:(fun i ->
         let board_index = Array.length board - 1 - i in
         let move = i + 1 in
         render_pit
           ~board_index
           ~is_goal:false
           ~player_class:"p1"
           ~move_number:(Some move)))
  in
  
  (* Bottom row: Player Two's pits *)
  let bottom_row =
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.id "bottom_row" ]
      (List.init game_state.num_squares_per_side ~f:(fun i ->
         let board_index = i + 1 in
         let move = i + 1 in
         render_pit
           ~board_index
           ~is_goal:false
           ~player_class:"p2"
           ~move_number:(Some move)))
  in
  
  let middle_rows =
    Vdom.Node.create "div" ~attrs:[ Vdom.Attr.id "middle_rows" ] [ top_row; bottom_row ]
  in
  
  (* Player 1 goal *)
  let p1_goal =
    render_pit ~board_index:0 ~is_goal:true ~player_class:"" ~move_number:None
  in
  
  (* Player 2 goal *)
  let p2_goal =
    render_pit
      ~board_index:(game_state.num_squares_per_side + 1)
      ~is_goal:true
      ~player_class:""
      ~move_number:None
  in
  
  (* Board *)
  let board_node =
    Vdom.Node.create "div" ~attrs:[ Vdom.Attr.id "board" ] [ p1_goal; middle_rows; p2_goal ]
  in
  
  (* Bottom numbers *)
  let bottom_numbers =
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.id "bottom_numbers" ]
      (List.init (Array.length board / 2 - 1) ~f:(fun i ->
         let board_index = i + 1 in
         Vdom.Node.create ~attrs:[] "p" [ Vdom.Node.text (Int.to_string board.(board_index)) ]))
  in
  
  (* Button panel *)
  let button_panel =
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.id "button_panel" ]
      [ Vdom.Node.create ~attrs:[] "button" [ Vdom.Node.text "Local Multiplayer" ]
      ; Vdom.Node.create ~attrs:[] "button" [ Vdom.Node.text "Player vs Computer" ]
      ; Vdom.Node.create ~attrs:[] "button" [ Vdom.Node.text "Cloud Multiplayer" ]
      ]
  in
  
  Vdom.Node.create
    "div"
    ~attrs:[ Vdom.Attr.class_ "game" ]
    [ hud; top_numbers; board_node; bottom_numbers; button_panel ]
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