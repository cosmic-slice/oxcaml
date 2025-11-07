(* Module for Firebase interaction with REST API *)

open! Core
open Js_of_ocaml

(* Characters available for the Game ID: A-Z, a-z, 0-9 *)
let id_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
let id_len = 6

(* Generates a random 6-character alphanumeric string *)
let generate_game_id () : string =
  let char_count = String.length id_chars in
  
  (* Generate a random integer in the range of valid chars *)
  let random_int () = 
    int_of_float (Js.to_float (Js.Unsafe.global##.Math##random) *. (float_of_int char_count))
  in
  let rec loop acc count =
    if count = 0 then acc
    else
      let index = random_int () in
      let char = id_chars.[index] in
      loop (String.of_char char ^ acc) (count - 1)
  in
  loop "" id_len

(* Simple HTTP request wrapper *)
module Http = struct
  let make_request ~http_method ~url ~body_opt ~callback =
    let xhr = XmlHttpRequest.create () in
    let method_str = Js.string http_method in
    let url_str = Js.string url in
    let async = Js._true in
    
    (* Open the connection *)
    xhr##_open method_str url_str async;
    
    (* Set content type *)
    xhr##setRequestHeader 
      (Js.string "Content-Type") 
      (Js.string "application/json");
    
    (* Set up callback for when request completes *)
    xhr##.onreadystatechange := Js.wrap_callback (fun _ ->
      (* Check if request is complete - readyState is a variant type *)
      match xhr##.readyState with
      | XmlHttpRequest.DONE ->
          let response_text = xhr##.responseText in
          let response_str = Js.Opt.case response_text
            (fun () -> "") (* If no response, return empty string *)
            (fun text -> Js.to_string text) (* If response exists, convert to string *)
          in
          callback response_str
      | _ -> ()
    );
    
    (* Send the request *)
    (match body_opt with
    | Some body_str -> 
        let body_js = Js.string body_str in
        xhr##send (Js.some body_js)
    | None -> 
        xhr##send Js.null);
    ()
    
  let get url callback = 
    make_request ~http_method:"GET" ~url ~body_opt:None ~callback
    
  let put url body callback = 
    make_request ~http_method:"PUT" ~url ~body_opt:(Some body) ~callback
    
  let patch url body callback = 
    make_request ~http_method:"PATCH" ~url ~body_opt:(Some body) ~callback
end

(* Your Firebase database URL *)
let database_url = "https://mancala-eefa4-default-rtdb.firebaseio.com"

(* Make a path to a Firebase endpoint *)
let make_url path = database_url ^ path ^ ".json"

(* Game data serialization *)
module Game_data = struct
  type player = PlayerOne | PlayerTwo
  
  let player_to_string = function
    | PlayerOne -> "PlayerOne"
    | PlayerTwo -> "PlayerTwo"
  
  let string_to_player = function
    | "PlayerOne" -> PlayerOne
    | "PlayerTwo" -> PlayerTwo
    | _ -> PlayerOne
  
  (* Convert board array to JSON string *)
  let board_to_json board =
    let items = Array.to_list board |> List.map ~f:Int.to_string in
    "[" ^ String.concat ~sep:"," items ^ "]"
  
  (* Parse board from JSON string (very basic parsing) *)
  let board_of_json json_str =
    try
      let cleaned = String.strip json_str ~drop:(fun c -> Char.(c = '[' || c = ']')) in
      String.split cleaned ~on:','
      |> List.map ~f:(fun s -> Int.of_string (String.strip s))
      |> Array.of_list
    with _ ->
      [||] (* Return empty array on parse error *)
  
  (* Create full game JSON manually *)
  let to_json ~game_id ~board ~current_player ~player1_id ~player2_id ~status ~timestamp =
    let p1_json = match player1_id with
      | Some id -> sprintf {|"player1Id":"%s"|} id
      | None -> {|"player1Id":null|}
    in
    let p2_json = match player2_id with
      | Some id -> sprintf {|"player2Id":"%s"|} id
      | None -> {|"player2Id":null|}
    in
    sprintf 
      {|{"gameId":"%s","board":%s,"currentPlayer":"%s",%s,%s,"status":"%s","timestamp":%f}|}
      game_id
      (board_to_json board)
      (player_to_string current_player)
      p1_json
      p2_json
      status
      timestamp
end

(* Create a new game *)
let create_game ~num_squares_per_side ~init_beads ~player_id ~callback =
  (* Get current timestamp using JavaScript Date *)
  let date_obj = Js.Unsafe.new_obj Js.Unsafe.global##._Date [||] in
  let timestamp = Js.to_float (Js.Unsafe.meth_call date_obj "getTime" [||]) in
  let game_id = generate_game_id () in
  
  (* Initialize board *)
  let board = Array.create ~len:((num_squares_per_side * 2) + 2) init_beads in
  board.(0) <- 0;
  board.(num_squares_per_side + 1) <- 0;
  
  (* Create JSON *)
  let json = Game_data.to_json
    ~game_id
    ~board
    ~current_player:PlayerOne
    ~player1_id:(Some player_id)
    ~player2_id:None
    ~status:"waiting"
    ~timestamp
  in
  
  (* Send to Firebase *)
  let url = make_url ("/games/" ^ game_id) in
  Http.put url json (fun _response ->
    let log_msg = Js.string ("Game created: " ^ game_id) in
    Firebug.console##log log_msg;
    callback game_id
  )

(* Join an existing game *)
let join_game ~game_id ~player_id ~callback =
  let json = sprintf {|{"player2Id":"%s","status":"active"}|} player_id in
  let url = make_url ("/games/" ^ game_id) in
  Http.patch url json (fun _response ->
    let log_msg = Js.string ("Joined game: " ^ game_id) in
    Firebug.console##log log_msg;
    callback ()
  )

(* Update game state after a move *)
let update_game_state ~game_id ~board ~current_player ~callback =
  let date_obj = Js.Unsafe.new_obj Js.Unsafe.global##._Date [||] in
  let timestamp = Js.to_float (Js.Unsafe.meth_call date_obj "getTime" [||]) in
  let json = sprintf 
    {|{"board":%s,"currentPlayer":"%s","timestamp":%f}|}
    (Game_data.board_to_json board)
    (Game_data.player_to_string current_player)
    timestamp
  in
  let url = make_url ("/games/" ^ game_id) in
  Http.patch url json (fun _response ->
    let log_msg = Js.string "Game state updated" in
    Firebug.console##log log_msg;
    callback ()
  )

(* Poll for game updates every 1 seconds *)
let start_polling ~game_id ~callback =
  let rec poll () =
    let url = make_url ("/games/" ^ game_id) in
    Http.get url (fun response ->
      (* Call the callback with the response *)
      callback response;
      
      (* Schedule next poll in 1 seconds (2000ms) *)
      let poll_callback = Js.wrap_callback poll in
      let timeout = 500.0 in
      ignore (Dom_html.window##setTimeout poll_callback timeout)
    )
  in
  poll ();
  (* Return a stop function (though we don't actually use it) *)
  (fun () -> ())

(* Helper to parse a simple field from JSON string in key:value pattern *)
let extract_json_field (json_str : string) field_name =
  try
    let pattern = sprintf {|"%s":"|} field_name in
    match String.substr_index json_str ~pattern with
    | Some start_idx ->
        let value_start = start_idx + String.length pattern in
        let rest = String.sub json_str ~pos:value_start ~len:(String.length json_str - value_start) in
        (match String.index rest '"' with
        | Some end_idx ->
            Some (String.sub rest ~pos:0 ~len:end_idx)
        | None -> None)
    | None -> None
  with _ -> None