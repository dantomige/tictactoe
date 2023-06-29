open! Core
open Tic_tac_toe_2023_common
open Tic_tac_toe_exercises_lib
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let possible_moves = available_moves ~game_kind ~pieces in
  List.random_element_exn possible_moves
;;

(* let possible_moves = () *)

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let possible_winning_moves = winning_moves ~me ~game_kind ~pieces in
  match possible_winning_moves with
  | [] -> random_move_strategy ~game_kind ~pieces
  | _ -> List.random_element_exn possible_winning_moves
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let possible_winning_moves = winning_moves ~me ~game_kind ~pieces in
  match possible_winning_moves with
  | [] ->
    let possible_blocking_moves = losing_moves ~me ~game_kind ~pieces in
    (match possible_blocking_moves with
     | [] -> random_move_strategy ~game_kind ~pieces
     | _ -> List.random_element_exn possible_blocking_moves)
  | _ -> List.random_element_exn possible_winning_moves
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_or_block_if_possible_strategy

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float * Evaluation.t
  =
  let game_state = evaluate ~game_kind ~pieces in
  match game_state with
  | Game_over { winner = player } ->
    (match player with
     | None -> 0.0, Game_over { winner = None }
     | Some player ->
       if Piece.equal player me
       then Float.infinity, Game_over { winner = Some player }
       else Float.neg_infinity, Game_over { winner = Some player })
  | Game_continues -> 0.0, Game_continues
  | Illegal_state -> 0.0, Illegal_state
;;

let _ = score

let rec minimax
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(depth : int)
  ~(is_maximizing_player : bool)
  : float * Position.t option
  =
  let score, game_evaluation = score ~me ~game_kind ~pieces in
  if depth = 0
     ||
     match game_evaluation with
     | Game_over _ -> true
     | Game_continues | Illegal_state -> false
  then (
    let node_value = score in
    node_value, None)
  else if is_maximizing_player (*Maximizing player*)
  then (
    let default_min = Float.neg_infinity, None in
    let possible_moves = available_moves ~game_kind ~pieces in
    (*List of tuples of the form (score, move_to_play)*)
    (*Getting all the possible moves and their score recursively*)
    let move_outcomes =
      List.map possible_moves ~f:(fun choice ->
        let new_pieces = Map.set pieces ~key:choice ~data:me in
        let next_node =
          minimax
            ~me
            ~game_kind
            ~pieces:new_pieces
            ~depth:(depth - 1)
            ~is_maximizing_player:(not is_maximizing_player)
        in
        fst next_node, Some choice)
    in
    List.fold
      move_outcomes
      ~init:default_min
      ~f:(fun curr_choice new_choice ->
      if Float.( >. ) (fst curr_choice) (fst new_choice)
      then curr_choice
      else new_choice))
  else (
    let default_max = Float.infinity, None in
    let possible_moves = available_moves ~game_kind ~pieces in
    (*List of tuples of the form (score, move_to_play)*)
    (*Getting all the possible moves and their score recursively*)
    let move_outcomes =
      List.map possible_moves ~f:(fun choice ->
        let new_pieces = Map.set pieces ~key:choice ~data:(Piece.flip me) in
        let next_node =
          minimax
            ~me
            ~game_kind
            ~pieces:new_pieces
            ~depth:(depth - 1)
            ~is_maximizing_player:(not is_maximizing_player)
        in
        fst next_node, Some choice)
    in
    List.fold
      move_outcomes
      ~init:default_max
      ~f:(fun curr_choice new_choice ->
      if Float.( <. ) (fst curr_choice) (fst new_choice)
      then curr_choice
      else new_choice))
;;

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  (* pick_winning_move_or_block_if_possible_strategy ~me
     ~game_kind:game_state.game_kind ~pieces:game_state.pieces *)
  let score, best_move =
    minimax
      ~me
      ~game_kind:game_state.game_kind
      ~pieces:game_state.pieces
      ~depth:3
      ~is_maximizing_player:true
  in
  let () =
    Async.print_s [%message (score : float) (best_move : Position.t option)]
  in
  match best_move with
  | None ->
    random_move_strategy
      ~game_kind:game_state.game_kind
      ~pieces:game_state.pieces
  | Some position -> position
;;
