open! Core
open Mancala_logic_library
open Hw2_mancala_logic
open Hw4_alpha_beta_search
open Virtual_dom
open! Bonsai.Let_syntax
open Js_of_ocaml

(* Defining a new struct to handle game modes *)
module Game_mode = struct 
  type t =
    | LocalMultiplayer
    | PlayerVsComputer
    | CloudMultiplayer
  [@@deriving sexp, compare, equal]
end

(* Cloud multiplayer state *)
module Cloud_state = struct
  type t = {
    player_id: string;
    current_game_id: string option;
    is_player_one: bool;
    game_id_input: string;
  } [@@deriving sexp, equal]
  
  let default () = {
    player_id = "player_" ^ Float.to_string (Js.to_float (Js.Unsafe.meth_call (Js.Unsafe.new_obj Js.Unsafe.global##._Date [||]) "getTime" [||]));
    current_game_id = None;
    is_player_one = true;
    game_id_input = "";
  }
end

(* Store the stop_polling function separately - not in state *)
let stop_polling_ref : (unit -> unit) option ref = ref None
(* Track last update timestamp to avoid duplicate updates *)
let last_update_timestamp : float ref = ref 0.0

(* All of the bead constants are declared here *)
let colors = [| "red"; "blue"; "green"; "yellow" |]
let bead_radius = 1.0
let distribution_radius = 25.0
let center_threshold = 6
let cx = 50.0
let cy = 50.0
let computerDepth = 3

let ids =
  [| "p1_goal"; "p2_1"; "p2_2"; "p2_3"; "p2_4"; "p2_5"; "p2_6"
    ; "p2_goal"; "p1_6"; "p1_5"; "p1_4"; "p1_3"; "p1_2"; "p1_1"
  |]
;;

(* --- SVG Rendering Functions --- *)
let create_bead_at ~x ~y ~radius ~color =
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
      let is_center_bead = num_beads = 1 || num_beads >= center_threshold in
      let dx, dy =
        if j = 0 && is_center_bead
        then 0.0, 0.0
        else (
          let num_distributed = if is_center_bead then num_beads - 1 else num_beads in
          let angle = 
            (j_float *. (2.0 *. Float.pi) /. float_of_int num_distributed) 
            +. pit_index_float 
          in
          let dx = distribution_radius *. Float.cos angle in
          let dy = distribution_radius *. Float.sin angle in
          dx, dy)
      in
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

(* Parse Firebase JSON response to extract board *)
let parse_board_from_json json_str =
  try
    (* Find the "board" field and extract the array *)
    let start_idx = match String.index json_str '[' with
      | Some idx -> idx
      | None -> raise_s [%message "No [ found"]
    in
    let end_idx = match String.index_from json_str start_idx ']' with
      | Some idx -> idx
      | None -> raise_s [%message "No ] found"]
    in
    let board_json = String.sub json_str ~pos:start_idx ~len:(end_idx - start_idx + 1) in
    Some (Firebase_rest.Game_data.board_of_json board_json)
  with _ -> None
;;

(* Extract timestamp from JSON *)
let parse_timestamp_from_json json_str =
  try
    match Firebase_rest.extract_json_field json_str "timestamp" with
    | Some ts_str -> Some (Float.of_string ts_str)
    | None -> None
  with _ -> None
;;

(* Cloud multiplayer panel *)
let cloud_multiplayer_panel ~cloud_state ~set_cloud_state ~set_game_state =
  match cloud_state.Cloud_state.current_game_id with
  | None ->
      (* Show game creation/joining interface *)
      Vdom.Node.create
        "div"
        ~attrs:[ Vdom.Attr.id "cloud_panel" ]
        [ Vdom.Node.create "h3" ~attrs:[] [ Vdom.Node.text "Cloud Multiplayer" ]
        ; Vdom.Node.create
            "button"
            ~attrs:
              [ Vdom.Attr.on_click (fun _ ->
                  Firebase_rest.create_game 
                    ~num_squares_per_side:6 
                    ~init_beads:4 
                    ~player_id:cloud_state.player_id
                    ~callback:(fun game_id ->
                      Firebug.console##log (Js.string ("Created game: " ^ game_id));
                      
                      (* Reset timestamp tracker *)
                      last_update_timestamp := 0.0;
                      
                      (* Start polling for updates *)
                      let stop_fn = Firebase_rest.start_polling ~game_id ~callback:(fun response ->
                        (* Check timestamp to avoid duplicate updates *)
                        match parse_timestamp_from_json response with
                        | Some timestamp when (Float.compare timestamp !last_update_timestamp) > 0 ->
                            last_update_timestamp := timestamp;
                            let current_player = 
                              match (Firebase_rest.extract_json_field response "currentPlayer") with
                              | Some "PlayerOne" -> Players.PlayerOne
                              | Some "PlayerTwo" -> Players.PlayerTwo
                              | _ -> Players.PlayerOne
                            in
                            (match parse_board_from_json response with
                            | Some new_board ->
                                let new_state = Game_state.create ~num_squares_per_side:6 ~init_beads:4 in
                                (match new_state with
                                | Ok state ->
                                    let updated_state = { state with board = new_board; decision = Playing { whose_turn = current_player }} in
                                    Ui_effect.Expert.handle (set_game_state updated_state)
                                | Error _ -> ())
                            | None -> ())
                        | _ -> ()
                      ) in
                      
                      stop_polling_ref := Some stop_fn;
                      
                      let new_cloud_state = 
                        { cloud_state with 
                          current_game_id = Some game_id;
                          is_player_one = true;
                        } 
                      in
                      Ui_effect.Expert.handle (set_cloud_state new_cloud_state)
                    );
                  Ui_effect.Ignore)
              ]
            [ Vdom.Node.text "Create New Game" ]
        ; Vdom.Node.create "p" ~attrs:[] [ Vdom.Node.text "Or join existing game:" ]
        ; Vdom.Node.create
            "input"
            ~attrs:
              [ Vdom.Attr.type_ "text"
              ; Vdom.Attr.placeholder "Enter Game ID"
              ; Vdom.Attr.value cloud_state.game_id_input
              ; Vdom.Attr.on_input (fun _ input_text ->
                  set_cloud_state { cloud_state with game_id_input = input_text })
              ]
            []
        ; Vdom.Node.create
            "button"
            ~attrs:
              [ Vdom.Attr.on_click (fun _ ->
                  if String.is_empty cloud_state.game_id_input then
                    Ui_effect.Ignore
                  else
                    let game_id = cloud_state.game_id_input in
                    Firebase_rest.join_game ~game_id ~player_id:cloud_state.player_id ~callback:(fun () ->
                      Firebug.console##log (Js.string ("Joined game: " ^ game_id));
                      
                      (* Reset timestamp tracker *)
                      last_update_timestamp := 0.0;
                      
                      (* Start polling for updates *)
                      let stop_fn = Firebase_rest.start_polling ~game_id ~callback:(fun response ->
                        (* Check timestamp to avoid duplicate updates *)
                        match parse_timestamp_from_json response with
                        | Some timestamp when (Float.compare timestamp !last_update_timestamp) > 0 ->
                            last_update_timestamp := timestamp;
                            let current_player = 
                              match (Firebase_rest.extract_json_field response "currentPlayer") with
                              | Some "PlayerOne" -> Players.PlayerOne
                              | Some "PlayerTwo" -> Players.PlayerTwo
                              | _ -> Players.PlayerOne
                            in
                            (match parse_board_from_json response with
                            | Some new_board ->
                                let new_state = Game_state.create ~num_squares_per_side:6 ~init_beads:4 in
                                (match new_state with
                                | Ok state ->
                                    let updated_state = { state with board = new_board; decision = Playing { whose_turn = current_player }} in
                                    Ui_effect.Expert.handle (set_game_state updated_state)
                                | Error _ -> ())
                            | None -> ())
                        | _ -> ()
                      ) in
                      
                      stop_polling_ref := Some stop_fn;
                      
                      let new_cloud_state = 
                        { cloud_state with 
                          current_game_id = Some game_id;
                          is_player_one = false;
                        } 
                      in
                      Ui_effect.Expert.handle (set_cloud_state new_cloud_state)
                    );
                    Ui_effect.Ignore)
              ]
            [ Vdom.Node.text "Join Game" ]
        ]
  | Some game_id ->
      (* Show current game info *)
      Vdom.Node.create
        "div"
        ~attrs:[ Vdom.Attr.id "cloud_panel" ]
        [ Vdom.Node.create "p" ~attrs:[] 
            [ Vdom.Node.text ("Game ID: " ^ game_id) ]
        ; Vdom.Node.create "p" ~attrs:[] 
            [ Vdom.Node.text ("You are: Player " ^ 
                (if cloud_state.is_player_one then "1" else "2")) ]
        ; Vdom.Node.create "p" ~attrs:[ Vdom.Attr.style (Css_gen.font_size (`Px 12)) ]
            [ Vdom.Node.text "Share the Game ID with your opponent" ]
        ; Vdom.Node.create
            "button"
            ~attrs:
              [ Vdom.Attr.on_click (fun _ ->
                  (* Stop polling if active *)
                  (match !stop_polling_ref with
                  | Some stop_fn -> stop_fn ()
                  | None -> ());
                  stop_polling_ref := None;
                  last_update_timestamp := 0.0;
                  set_cloud_state { cloud_state with current_game_id = None })
              ]
            [ Vdom.Node.text "Leave Game" ]
        ]
;;

let mancala_board ~(game_state : Game_state.t) ~set_game_state 
    ~(game_mode : Game_mode.t) ~set_game_mode
    ~(cloud_state : Cloud_state.t) ~set_cloud_state =
  let is_game_over = Game_state.is_game_over game_state in
  let board = game_state.board in

  let delay n =
    let rec wait n = if n <= 0 then () else wait (n - 1) in
    wait (n * 1000000)
  in

  let handle_move (new_game_state : Game_state.t) =
    match game_mode with
    | Game_mode.PlayerVsComputer ->
        let rec make_ai_moves_if_needed (current_state : Game_state.t) =
          match current_state.decision with
          | Playing { whose_turn = Players.PlayerTwo } ->
              delay 5;
              let ai_move = alpha_beta current_state ~depth:computerDepth |> Option.value_exn in
              (match Game_state.make_move current_state ai_move with
              | Error _ -> raise_s [%message "AI move failed" (ai_move : int)]
              | Ok ai_game_state -> 
                  set_game_state ai_game_state :: make_ai_moves_if_needed ai_game_state)
          | _ -> []
        in
        let all_effects = set_game_state new_game_state :: make_ai_moves_if_needed new_game_state in
        Ui_effect.Many all_effects
    | Game_mode.CloudMultiplayer ->
      (* Update Firebase with new game state AND update local state immediately *)
      (match cloud_state.current_game_id with
      | Some game_id ->
        let current_player = 
        match new_game_state.decision with
        | Playing { whose_turn = Players.PlayerOne } -> Firebase_rest.Game_data.PlayerOne
        | Playing { whose_turn = Players.PlayerTwo } -> Firebase_rest.Game_data.PlayerTwo
        | Winner Players.PlayerOne -> Firebase_rest.Game_data.PlayerOne
        | Winner Players.PlayerTwo -> Firebase_rest.Game_data.PlayerTwo
        | Tie -> Firebase_rest.Game_data.PlayerOne
        in
        (* Update timestamp to prevent re-applying our own move *)
        let date_obj = Js.Unsafe.new_obj Js.Unsafe.global##._Date [||] in
        let timestamp = Js.to_float (Js.Unsafe.meth_call date_obj "getTime" [||]) in
        last_update_timestamp := timestamp;
        
        Firebase_rest.update_game_state 
        ~game_id 
        ~board:new_game_state.board 
        ~current_player
        ~callback:(fun () ->
          Firebug.console##log (Js.string "Move synced to Firebase")
        );
        (* Update local state immediately for responsive UI *)
        set_game_state new_game_state
      | None -> 
        set_game_state new_game_state)
    | _ -> 
      set_game_state new_game_state
  in

  let render_pit ~board_index ~is_goal ~player_class ~move_number =
    let num_beads = board.(board_index) in
    let id_class = ids.(board_index) in
    let beads_svg = render_beads ~num_beads ~pit_index:board_index in
    
    let belongs_to_player =
      match player_class, game_state.decision, game_mode with
      | "p1", Playing { whose_turn = Players.PlayerOne }, _ -> true
      | "p2", Playing { whose_turn = Players.PlayerTwo }, Game_mode.LocalMultiplayer -> true
      | "p2", Playing { whose_turn = Players.PlayerTwo }, Game_mode.CloudMultiplayer -> true
      | _ -> false
    in
    
    (* For cloud multiplayer, only allow moves if you're the right player *)
    let can_move_in_cloud =
      match game_mode with
      | Game_mode.CloudMultiplayer -> (
        match cloud_state.is_player_one, player_class with
        | true, "p1" -> true
        | false, "p2" -> true
        | _, _ -> false
      )
      | _ -> true
    in
    
    let maybe_clickable_attr =
      if (not belongs_to_player) || is_game_over || is_goal || 
         Option.is_none move_number || not can_move_in_cloud
      then Vdom.Attr.empty
      else (
        let move = Option.value_exn move_number in
        Vdom.Attr.on_click (fun _ ->
          match Game_state.make_move game_state move with
          | Error _ -> raise_s [%message "Invalid move" (move : int)]
          | Ok new_game_state -> handle_move new_game_state))
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
  
  let player_label =
    match game_mode with
    | Game_mode.PlayerVsComputer -> "AI"
    | _ -> "PLAYER 2"
  in

  let player2_score =
    Vdom.Node.create
      "p"
      ~attrs:[ Vdom.Attr.id "player2_score" ]
      [ Vdom.Node.create ~attrs:[] "strong" [ Vdom.Node.text player_label ]
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
  
  let top_numbers =
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.id "top_numbers" ]
      (List.init (Array.length board / 2 - 1) ~f:(fun i ->
         let board_index = Array.length board - 1 - i in
         Vdom.Node.create ~attrs:[] "p" [ Vdom.Node.text (Int.to_string board.(board_index)) ]))
  in
  
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
  
  let p1_goal =
    render_pit ~board_index:0 ~is_goal:true ~player_class:"" ~move_number:None
  in
  
  let p2_goal =
    render_pit
      ~board_index:(game_state.num_squares_per_side + 1)
      ~is_goal:true
      ~player_class:""
      ~move_number:None
  in
  
  let board_node =
    Vdom.Node.create "div" ~attrs:[ Vdom.Attr.id "board" ] [ p1_goal; middle_rows; p2_goal ]
  in
  
  let bottom_numbers =
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.id "bottom_numbers" ]
      (List.init (Array.length board / 2 - 1) ~f:(fun i ->
         let board_index = i + 1 in
         Vdom.Node.create ~attrs:[] "p" [ Vdom.Node.text (Int.to_string board.(board_index)) ]))
  in

  let button_panel =
    let create_mode_button ~label ~mode ~button_id =
      let is_active = Game_mode.equal game_mode mode in
      Vdom.Node.create
        "button"
        ~attrs:
          [ Vdom.Attr.id button_id
          ; (if is_active then Vdom.Attr.classes ["active"] else Vdom.Attr.empty)
          ; Vdom.Attr.on_click (fun _ -> 
            (* Stop polling if switching away from cloud mode *)
            (if Game_mode.equal game_mode Game_mode.CloudMultiplayer then (
              match !stop_polling_ref with
              | Some stop_fn -> 
                  stop_fn ();
                  Firebug.console##log (Js.string "Stopped polling")
              | None -> ()
            ));
            stop_polling_ref := None;
            last_update_timestamp := 0.0;
            
            let init_state = 
              Game_state.create ~num_squares_per_side:6 ~init_beads:4
              |> Result.ok
              |> Option.value_exn
            in
            Ui_effect.Many 
              [ set_game_mode mode
              ; set_game_state init_state
              ; set_cloud_state (Cloud_state.default ())
              ])
          ]
        [ Vdom.Node.text label ]
    in
    Vdom.Node.create
      "div"
      ~attrs:[ Vdom.Attr.id "button_panel" ]
      [ create_mode_button ~label:"Local Multiplayer" ~mode:Game_mode.LocalMultiplayer ~button_id:"localMultiplayer"
      ; create_mode_button ~label:"Player vs Computer" ~mode:Game_mode.PlayerVsComputer ~button_id:"playerVsComputer"
      ; create_mode_button ~label:"Cloud Multiplayer" ~mode:Game_mode.CloudMultiplayer ~button_id:"cloudMultiplayer"
      ]
  in

  (* Add cloud multiplayer panel when in that mode *)
  let cloud_panel =
    if Game_mode.equal game_mode Game_mode.CloudMultiplayer
    then cloud_multiplayer_panel ~cloud_state ~set_cloud_state ~set_game_state
    else Vdom.Node.none
  in

  Vdom.Node.create
    "div"
    ~attrs:[ Vdom.Attr.class_ "game" ]
    [ hud; cloud_panel; top_numbers; board_node; bottom_numbers; button_panel ]
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
  let%sub game_mode, set_game_mode =
    Bonsai.state ~default_model:Game_mode.LocalMultiplayer (module Game_mode)
  in
  let%sub cloud_state, set_cloud_state =
    Bonsai.state ~default_model:(Cloud_state.default ()) (module Cloud_state)
  in
  let%arr game_state = game_state
  and set_game_state = set_game_state
  and game_mode = game_mode
  and set_game_mode = set_game_mode
  and cloud_state = cloud_state
  and set_cloud_state = set_cloud_state in
  mancala_board ~game_state ~set_game_state ~game_mode ~set_game_mode
    ~cloud_state ~set_cloud_state
;;

let () = Bonsai_web.Start.start app