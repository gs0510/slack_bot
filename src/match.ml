open Yojson.Basic
open Yojson.Basic.Util

let () = Random.init (int_of_float (Unix.time ()))

let shuffle list =
  let nd =
    List.map
      (fun c ->
        let random = Random.bits () in
        (random, c))
      list
  in
  let sond = List.sort compare nd in
  List.map snd sond

let rec pair_up_list (acc : string list list) (members : string list) :
    string list list =
  match members with
  | [] -> acc
  | [ last ] -> (
      match acc with
      | [] -> failwith "There's only one person in this channel."
      | fst :: tl -> (last :: fst) :: tl)
  | f :: s :: tl -> pair_up_list ([ f; s ] :: acc) tl

let to_string (matches_list : string list list) =
  (List.map (List.map (fun member -> "<@" ^ member ^ ">")) matches_list
  |> List.fold_left
       (fun acc current_match ->
         acc ^ String.concat " with " current_match ^ "\n")
       ":coffee: Matches this week:\n")
  ^ "\n\
    \ Have some nice coffee chats! \n\
    \ :sheepy: :sheepy: :sheepy: :sheepy: :sheepy: :sheepy: :sheepy: :sheepy: \
     :sheepy:\n\
     Remember that I (the :coffee:bot) don't initiate a conversation. You'll \
     have to reach out to your coffee chat partner by yourself:writing_hand:"

let get_most_optimum max_iters db_path =
  let channel =
    from_file "config" |> member "test_channel_id"
    |> Yojson.Basic.Util.to_string
  in
  let _ = print_endline channel in
  let members =
    match Lwt_main.run (Curl_requests.get_reactions channel) with
    | Error _ -> assert false
    | Ok members -> members
  in
  let tbl = Score.construct_hashmap (Irmin_io.get_old_matches db_path) in
  let rec loop num_iter best_match best_score =
    if num_iter = max_iters then
      let _ = Printf.printf "\n Number iterations: %d \n" num_iter in
      best_match
    else
      let new_match = members |> shuffle |> pair_up_list [] in
      let new_score = Score.compute_total tbl new_match in
      match new_score with
      | 0 ->
          let _ = Printf.printf "\n Number iterations: %d \n" num_iter in
          new_match
      | _ ->
          if new_score < best_score then loop (num_iter + 1) new_match new_score
          else loop (num_iter + 1) best_match best_score
  in
  let first_match = members |> shuffle |> pair_up_list [] in
  loop 1 first_match (Score.compute_total tbl first_match)
