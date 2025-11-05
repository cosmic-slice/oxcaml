(* Empty on purpose (the ml is the "executable", i.e., it'll be compiled in JavaScript.) *)
module Game_mode : sig
    type t =
        | LocalMultiplayer
        | PlayerVsComputer
        | CloudMultiplayer
    [@@deriving sexp, compare, equal]
end