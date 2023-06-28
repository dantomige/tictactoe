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

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

let all_slots ~(game_kind : Game_kind.t) : Position.t list =
  let board_length = Protocol.Game_kind.board_length game_kind in
  let all_moves =
    List.init board_length ~f:(fun row ->
      List.init board_length ~f:(fun col -> { Position.row; column = col }))
  in
  List.concat all_moves
;;

let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let all_pos = all_slots ~game_kind in
  let filled_pos = Map.keys pieces in
  List.filter all_pos ~f:(fun pos ->
    not (List.mem filled_pos pos ~equal:Position.equal))
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

let check_for_game_win 
  (curr_pos: Position.t) 
  (game_kind: Game_kind.t)
  (pieces: Piece.t Position.Map.t) 
  (piece_type: Piece.t): bool =

  let win_length = Game_kind.win_length game_kind in
  
  let up, right, up_right, down_right = 
    Position.up, 
    Position.right,
    (fun position -> position |> Position.up |> Position.right),
    (fun position -> position |> Position.down |> Position.right) in 

  let rec check_direction location direction piece_type length =
    match Map.find pieces location with 
    | None -> false
    | Some piece -> 
      if (Piece.equal piece_type piece)
        then
          (if length = 1 then true else check_direction (direction location) direction piece_type (length - 1))
        else
          false in

  let direction_list = [up; right; up_right; down_right] in
  
  let rec check_all_directions start_location piece_type direction_list = 
    match direction_list with
    | [] -> false
    | direction :: rest -> check_direction start_location direction piece_type win_length 
    || check_all_directions start_location piece_type rest in 
  
  check_all_directions curr_pos piece_type direction_list

;;
  

let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  let all_positions = Map.keys pieces in
  let board_length = Game_kind.board_length game_kind in

  let x_positions = List.filter ~f:(fun pos -> match Map.find pieces pos with |None| Some O -> false |Some X -> true) all_positions in
  let o_positions = List.filter ~f:(fun pos -> match Map.find pieces pos with |None| Some O -> false |Some X -> true) all_positions in
  
  let rec search_positions_for_win positions (pieces: Piece.t Position.Map.t) (piece_type: Piece.t)=
    match (positions) with 
    | [] -> false
    | curr_position :: rest -> check_for_game_win curr_position game_kind pieces piece_type || search_positions_for_win rest pieces piece_type in
    
  let is_x_victory = search_positions_for_win x_positions pieces (X: Piece.t) in 
  let is_o_victory = search_positions_for_win o_positions pieces (O: Piece.t) in 

  match is_x_victory, is_o_victory with
  | true, false -> Game_over {winner = Some X}
  | false, true -> Game_over {winner = Some O}
  | true, true -> Illegal_state
  | false, false -> if (Map.length pieces = board_length * board_length) then (Game_over {winner = None})
   else Game_continues
  
    (* TRIED THIS SOLUTION AND FAILED *)
  (* let win_length = Protocol.Game_kind.win_length game_kind in
  let board_length = Protocol.Game_kind.win_length game_kind in

  let universal_evaluator ~(dir_of_evaluation) ~(dir_of_traversal) ~(start) =

    (* Creating the helper function to create the evaluation array *)
    let rec eval_start_pos_creator length pos = match board_length with
    | 0 -> []
    | _ -> pos :: eval_start_pos_creator (length - 1) (dir_of_evaluation pos) in

    (* Creating an array where each element represents the start of a new col/row/diag *)
    let eval_start_pos = eval_start_pos_creator board_length start in

    (* Finds all the starting positions for the windows in a given direction of traversal**)
    let traversal_start_pos_creator start = 

      (** Finds the end of a window. Necessary to check if a given window is inbounds*)
      let rec find_end_of_window length pos = match length with 
        | 1 -> pos
        | _ -> find_end_of_window (length - 1) (dir_of_traversal pos) in 

      (** Boolean to determine if a window is inbounds*)
      let in_bounds length start = 
        let end_pos = find_end_of_window length start in
        if Position.in_bounds end_pos ~game_kind then true else false in

      let rec final_traversal_start_pos_creator (pos : Position.t) =
        match (in_bounds win_length pos: bool) with 
        | true -> pos :: final_traversal_start_pos_creator (dir_of_traversal pos)
        | false -> [] in

      final_traversal_start_pos_creator start in

      
    let all_starting_pos = List.concat (List.map ~f:traversal_start_pos_creator eval_start_pos) in

    (* A function to determine if a sliding window has a winner and if so who that winner is *)
    let check_window_winner pos = 
      let rec create_sliding_window window_length curr_pos = match window_length with 
      | 0 -> []
        (* Returns the piece at the current position *)
      | _ -> (Map.find pieces curr_pos) :: create_sliding_window (window_length - 1) (dir_of_traversal curr_pos) in 
      
      (** Checks if a window isnt filled i.e. it has None *)
      let curr_window = create_sliding_window win_length pos in
      let piece_equality left right = match left, right with 
      | Some left, Some right -> Protocol.Piece.equal left right
      | None, None -> true
      | _, _ -> false in

      (**Winning array*)
      let winning_array (piece: Piece.t) : Piece.t option list = List.init win_length ~f:(fun x -> Some piece) in
      
      (* *Is the list not filled all the away? *)
      match List.mem curr_window None piece_equality with
      | true -> None 
      | false -> 
      if List.equal piece_equality (winning_array Protocol.Piece.O) curr_window  
        then Some Protocol.Piece.O
      else if List.equal piece_equality (winning_array Protocol.Piece.X) curr_window 
        then Some Protocol.Piece.X
      else None in 
    
      (* Checks each of the windows for a window, filters out None *)
    let all_windows = List.filter 
      (List.map all_starting_pos ~f:check_window_winner) (fun x -> piece_equality None x) in
    
    match List.mem all_windows (Some Protocol.Piece.O), List.mem all_windows (Some Protocol.Piece.X) with 
    | false, false -> Protocol.Evaluation.Game_continues
    | true, false -> Protocol.Evaluation.Game_over of {winner: Protocol.Piece.O}
    | false, true -> Protocol.Evaluation.Game_over of {winner: Protocol.Piece.X}
    | true, true -> Protocol.Evaluation.Illegal_state  *)


      (** Matching whether each piece exists in a list*)
;;

(* let win_length = Protocol.Game_kind.win_length game_kind in win_length *)

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
    match (evaluate ~game_kind ~pieces:updated_pieces) with 
    | Game_over {winner = state} -> (match state with |Some winner -> Piece.equal winner me| _ -> false) 
    | Illegal_state -> false
    | Game_continues -> false

  in List.filter possible_moves ~f:test_winning_moves
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
let%expect_test "evalulate_win_for_x" = print_endline (evaluate
   ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces |>
   Evaluation.to_string); [%expect {| (Game_over(winner(X))) |}] ;;

   let%expect_test "evalulate_non_win" = print_endline (evaluate
   ~game_kind:non_win.game_kind ~pieces:non_win.pieces |>
   Evaluation.to_string); [%expect {| Game_continues |}] ;;

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
let%expect_test "winning_move" = let positions = winning_moves
  ~game_kind:non_win.game_kind ~pieces:non_win.pieces ~me:Piece.X in print_s
  [%sexp (positions : Position.t list)]; [%expect {| (((row 1) (column 1)))
  |}]; let positions = winning_moves ~game_kind:non_win.game_kind
  ~pieces:non_win.pieces ~me:Piece.O in print_s [%sexp (positions :
  Position.t list)]; [%expect {| () |}] ;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
let%expect_test "print_losing" = let positions = losing_moves
  ~game_kind:non_win.game_kind ~pieces:non_win.pieces ~me:Piece.X in print_s
  [%sexp (positions : Position.t list)]; [%expect {| () |}]; let positions =
  losing_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
  ~me:Piece.O in print_s [%sexp (positions : Position.t list)]; [%expect {|
  (((row 1) (column 1))) |}] ;;
