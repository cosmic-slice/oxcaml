open! Core

type role = 
  | Warrior
  | Mage
  | Cleric
  | Archer

type tile_type = 
  | Start
  | Combat
  | Treasure
  | Random
  | Shop

type stats = 
  | HP
  | Strength
  | Defense
  | Luck

type direction = 
  | North
  | South
  | East
  | West

type coordinate = {
  x: int;
  y: int
}

type decision = 
  | Playing
  | Winner of { player_id : int }
  | Game_Over

type tile = tile_type * direction

type Player = {
  id: int;
  name: string;
  role: role;
  hp: int;
  max_hp: int;
  gold: int;
  position: coordinate;
  deck: Card list
}

type Card = {
  name: string;
  description: string;
  effects: (stats * int) list
}

type Board = {
  tile_map: tile list list;
  width: int;
  height: int;
}

type game_state = {
  board: Board;
  active_players: Player list;
  decision: decision
}

let initial_state : game_state = {
  board: ;
  active_players: [| p1_init ; p2_init |];
  decision: Playing
}

let intermediate_state : game_state = {
  board: ;
  active_players: [];
  decision: Playing
}

let final_state : game_state = {
  board: ;
  active_players: [];
  decision: Winner 1
}

let p1_init = {
  id: 1;
  name: "Alex";
  role: role.Warrior;
  hp: 50;
  max_hp: 50;
  gold: 500;
  position: {x: 0; y: 0};
  deck: []
}

let p2_init = {
  id: 2;
  name: "John";
  role: role.Archer;
  hp: 50;
  max_hp: 50;
  gold: 500;
  position: {x: 0; y: 0};
  deck: []
}