open Core
open Tic_tac_toe_2023_common
open Protocol

(** This file contains exercises 1-4. For instructions refer to the README at
    the root of this repository. *)

(* Returns a list of all coordinates in a board type.
val all_slot
  : game_kind:Game_kind.t 
  -> Position.t list *)

(** Finds all available/empty slots in a tic tac toe/omok game. *)
val available_moves
  :  game_kind:Game_kind.t
  -> pieces:Piece.t Position.Map.t
  -> Position.t list

module Evaluation : sig
  (* Represents the evaluation of a tic tac toe / omok game. *)
  type t =
    | Illegal_state
    | Game_over of { winner : Protocol.Piece.t option }
    | Game_continues

  val to_string : t -> string
end

(** Evaluates the single state of an Omok/Tic Tac Toe game. *)
val evaluate
  :  game_kind:Protocol.Game_kind.t
  -> pieces:Protocol.Piece.t Protocol.Position.Map.t
  -> Evaluation.t

(** Finds all of the moves that would win in the next move. *)
val winning_moves
  :  me:Piece.t
  -> game_kind:Game_kind.t
  -> pieces:Piece.t Position.Map.t
  -> Position.t list

(** Finds all of the winning moves for your opponent. *)
val losing_moves
  :  me:Piece.t
  -> game_kind:Game_kind.t
  -> pieces:Piece.t Position.Map.t
  -> Position.t list


(** The below commands provide debugging/visibility into the different
    exercises for your bot. *)
val exercise_one : Command.t

val exercise_two : Command.t
val exercise_three : Command.t
val exercise_four : Command.t
