open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

let test1_win_for_o =
  empty_game
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

(*TIC TAC TOE*)
let all_slots ~(game_kind : Game_kind.t) : Position.t list =
  let board_length = Game_kind.board_length game_kind in
  let all_moves =
    List.init board_length ~f:(fun row ->
      List.init board_length ~f:(fun col -> { Position.row; column = col }))
  in
  List.concat all_moves
;;

let tic_tac_toe_available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let all_pos = all_slots ~game_kind in
  let filled_pos = Map.keys pieces in
  List.filter all_pos ~f:(fun pos ->
    not (List.mem filled_pos pos ~equal:Position.equal))
;;

(*OMOK*)
(* Gets all adjecent moves that are inbounds from a given position*)
let rec apply_all_offsets
  (pos : Position.t)
  current_offsets
  game_kind
  occupied_spaces
  =
  match current_offsets with
  | [] -> []
  | dir :: other_offsets ->
    if Position.in_bounds (dir pos) ~game_kind
       && not (Set.mem occupied_spaces (dir pos))
    then
      dir pos
      :: apply_all_offsets pos other_offsets game_kind occupied_spaces
    else apply_all_offsets pos other_offsets game_kind occupied_spaces
;;

(*Gets the set of all adjacent moves given a list of all curent pieces*)
let rec add_all_moves
  (possible_moves : Position.Set.t)
  (piece_positions : Position.t list)
  (game_kind : Game_kind.t)
  occupied_positions
  =
  let all_offsets = Position.all_offsets in
  match piece_positions with
  | [] -> possible_moves
  | piece :: other_pieces ->
    let new_set =
      List.fold_right
        (apply_all_offsets piece all_offsets game_kind occupied_positions)
        ~f:(fun elem acc -> Set.add acc elem)
        ~init:possible_moves
    in
    add_all_moves new_set other_pieces game_kind occupied_positions
;;

let omok_available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let current_piece_positions = Map.keys pieces in
  let set_of_current_piece_positions =
    List.fold current_piece_positions ~init:Position.Set.empty ~f:Set.add
  in
  let set_of_available_moves =
    add_all_moves
      Position.Set.empty
      current_piece_positions
      game_kind
      set_of_current_piece_positions
  in
  let list_of_available_moves = Set.to_list set_of_available_moves in
  match list_of_available_moves with
  | [] -> [ { Position.row = 7; column = 7 } ]
  | _ -> list_of_available_moves
;;

let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  match game_kind with
  | Tic_tac_toe -> tic_tac_toe_available_moves ~game_kind ~pieces
  | Omok -> omok_available_moves ~game_kind ~pieces
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

let check_for_game_win
  (curr_pos : Position.t)
  (game_kind : Game_kind.t)
  (pieces : Piece.t Position.Map.t)
  (piece_type : Piece.t)
  : bool
  =
  let win_length = Game_kind.win_length game_kind in
  let up, right, up_right, down_right =
    ( Position.up
    , Position.right
    , (fun position -> position |> Position.up |> Position.right)
    , fun position -> position |> Position.down |> Position.right )
  in
  let rec check_direction location direction piece_type length =
    match Map.find pieces location with
    | None -> false
    | Some piece ->
      if Piece.equal piece_type piece
      then
        length = 1
        || check_direction
             (direction location)
             direction
             piece_type
             (length - 1)
      else false
  in
  let universal_direction_list = [ up; right; up_right; down_right ] in
  let rec check_all_directions start_location piece_type direction_list =
    match direction_list with
    | [] -> false
    | direction :: rest ->
      check_direction start_location direction piece_type win_length
      || check_all_directions start_location piece_type rest
  in
  check_all_directions curr_pos piece_type universal_direction_list
;;

let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  let all_positions = Map.keys pieces in
  let board_length = Game_kind.board_length game_kind in
  let x_positions =
    List.filter
      ~f:(fun pos ->
        match Map.find pieces pos with
        | None | Some O -> false
        | Some X -> true)
      all_positions
  in
  let o_positions =
    List.filter
      ~f:(fun pos ->
        match Map.find pieces pos with
        | None | Some X -> false
        | Some O -> true)
      all_positions
  in
  let rec search_positions_for_win
    positions
    (pieces : Piece.t Position.Map.t)
    (piece_type : Piece.t)
    =
    match positions with
    | [] -> false
    | curr_position :: rest ->
      check_for_game_win curr_position game_kind pieces piece_type
      || search_positions_for_win rest pieces piece_type
  in
  let is_x_victory =
    search_positions_for_win x_positions pieces (X : Piece.t)
  in
  let is_o_victory =
    search_positions_for_win o_positions pieces (O : Piece.t)
  in
  match is_x_victory, is_o_victory with
  | true, false -> Game_over { winner = Some X }
  | false, true -> Game_over { winner = Some O }
  | true, true -> Illegal_state
  | false, false ->
    if Map.length pieces = board_length * board_length
    then Game_over { winner = None }
    else Game_continues
;;

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let possible_moves = available_moves ~game_kind ~pieces in
  let test_winning_moves move =
    let updated_pieces = Map.set pieces ~key:move ~data:me in
    match evaluate ~game_kind ~pieces:updated_pieces with
    | Game_over { winner = state } ->
      (match state with Some winner -> Piece.equal winner me | _ -> false)
    | Illegal_state -> false
    | Game_continues -> false
  in
  List.filter possible_moves ~f:test_winning_moves
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let opponent = Piece.flip me in
  winning_moves ~me:opponent ~game_kind ~pieces
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

(* After you've implemented [available_moves], uncomment these tests! *)
let%expect_test "yes available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect
    {| 
   (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1))
    ((row 1) (column 2)) ((row 2) (column 1))) |}]
;;

let%expect_test "no available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)
let%expect_test "evalulate_win_for_x" =
  print_endline
    (evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evalulate_non_win" =
  print_endline
    (evaluate ~game_kind:non_win.game_kind ~pieces:non_win.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

let%expect_test "evalulate_test1_win_for_o" =
  print_endline
    (evaluate
       ~game_kind:test1_win_for_o.game_kind
       ~pieces:test1_win_for_o.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(O))) |}]
;;

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
let%expect_test "winning_move" =
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| (((row 1) (column 1)))
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" =
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {| () |}];
  let positions =
    losing_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {|
  (((row 1) (column 1))) |}]
;;

let%expect_test "print time" =
  print_endline (Game_state.to_string_hum test1_win_for_o);
  [%expect {||}]
;;
