open Yojson.Basic
open Yojson.Basic.Util

type matches = { matched : string list list } [@@deriving yojson]

let config = from_file "config"

let token = member "token" config |> to_string

let channel = member "channel_id" config |> to_string

let test_channel = member "test_channel_id" config |> to_string

let bot_id = member "bot_id" config |> to_string

let args_channel = [ "-F"; "token=" ^ token; "-F"; "channel=" ^ channel ]

let members =
  ( match
      Curly.(
        run ~args:args_channel
          (Request.make ~url:"https://slack.com/api/conversations.members"
             ~meth:`POST ()))
    with
  | Ok x ->
      List.map to_string
        ( from_string x.Curly.Response.body
        |> Util.member "members" |> Util.to_list )
  | _ -> failwith "There's an error" )
  |> List.filter (fun id ->
         id <> bot_id && id <> "U0J5U03J4" (*avsm*) && id <> "U0JP4EH7H" (*samoht*) && id <> "U0JCSR1HT" (*magnus*) && id <> "UEQMNGNH0" (* pascutto*))

let random_init = Random.init (int_of_float (Unix.time ()))

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

let rec match_list (output : string list list) (users : string list) :
    string list list =
  match users with
  | [] -> output
  | [ last ] -> (
      match output with
      | [] -> failwith "There's only one person in this channel."
      | fst :: tl -> (last :: fst) :: tl )
  | f :: s :: tl -> match_list ([ f; s ] :: output) tl


let create_output (matches_list : string list list) =
  (List.map (List.map (fun member ->"<@" ^ member ^ ">" )) matches_list |>
  List.fold_left
    (fun acc current_match -> acc ^ String.concat " with " current_match ^ "\n")
    ":coffee: Matches this week:\n") ^ "\n Have some nice coffee chats! :grinning:"

module Git_store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

let info message = Irmin_unix.info ~author:"Example" "%s" message

open Lwt.Infix


let all_old_matches db_path =
  let git_config = Irmin_git.config ~bare:true db_path in
  let  epoch_list =
    Git_store.Repo.v git_config
    >>= Git_store.master
    >>= (fun t ->
         (* todo: also handle the case of directories with a log*)
          Git_store.list t [ "matches" ] >|= List.map (fun (step, _) -> step))
    |> Lwt_main.run
  in
  let matches = Git_store.Repo.v git_config
  >>= Git_store.master
  >>= (fun t ->
        Lwt_list.map_s
          (fun epoch ->  Git_store.get t [ "matches"; epoch ] )
          epoch_list)
  |> Lwt_main.run in 
  List.combine epoch_list matches


let parsing_json all_matches_json = to_list all_matches_json |> List.map to_list

let order_pair uid1 uid2 = if uid1 < uid2 then (uid1, uid2) else (uid2, uid1)
let update_key uid1 uid2 tbl value =
  let pair =  order_pair uid1 uid2 in
  match Hashtbl.find_opt tbl pair with
  | Some num_matches -> Hashtbl.replace tbl pair (num_matches + value)
  | None -> Hashtbl.add tbl pair value

let get_score uid1 uid2 tbl =
  let pair = order_pair uid1 uid2 in
  match Hashtbl.find_opt tbl pair with
  | Some num_matches -> num_matches
  | None -> 0

(* let rec fold_over_2 l1 l2 acc=
let add_pair_avoiding_dupl mem1 mem1 list =
  if mem2 <> mem1 then let ordered_pair = order_pair mem2 mem1 in ::inner_acc else inner_acc) in
  match l1 with 
| mem1::tl -> List.append acc (List.fold_left (fun inner_acc mem2-> if mem2 <> mem1 then (order_pair mem2 mem1)::inner_acc else inner_acc) [] l2) |> fold_over_2 tl l2
| [] -> acc *)

let single_match_score epoch =
     let now = Unix.time () in
     let value = float_of_string epoch in
     let day = 86400. in
     if (now-.value)/. day <= 9. then 5
     else if (now-. value) /. day  <= 16. then 3
     else 1  
    

(* TODO: make the following two functions somehow reasonable!!!! xD *)
let construct_hashmap all_old_matches =
  let tbl = Hashtbl.create 256 in
  List.iter
    (fun (epoch, match_json) ->
      let value = single_match_score epoch in
     from_string match_json |> member "matched" |> parsing_json
      |> List.iter (fun current_match ->
             match List.length current_match with
             | 2 ->
                 update_key
                   (List.nth current_match 0 |> to_string)
                   (List.nth current_match 1 |> to_string)
                   tbl value
             | 3 ->
                 update_key
                   (List.nth current_match 0 |> to_string)
                   (List.nth current_match 1 |> to_string)
                   tbl value;
                 update_key
                   (List.nth current_match 1 |> to_string)
                   (List.nth current_match 2 |> to_string)
                   tbl value;
                 update_key
                   (List.nth current_match 0 |> to_string)
                   (List.nth current_match 2 |> to_string)
                   tbl value
             | _ -> failwith "not accounted for!"))
    all_old_matches;
  tbl

let compute_total_score tbl matches =
  List.fold_left
    (fun score current_match ->
      let pair_score =
        match List.length current_match with
        | 2 ->
            get_score (List.nth current_match 0) (List.nth current_match 1) tbl
        | 3 ->
            get_score (List.nth current_match 0) (List.nth current_match 1) tbl
            + get_score (List.nth current_match 1) (List.nth current_match 2)
                tbl
            + get_score (List.nth current_match 0) (List.nth current_match 2)
                tbl
        | _ -> failwith "not accounted for!"
      in
      score + pair_score)
    0 matches

let calculate_most_optimum_match num_times tbl =
  let rec loop num_iter best_match best_score =
    if num_iter = num_times then let _ = Printf.printf "\n Number iterations: %d \n" num_iter in best_match
    else
      let new_match = shuffle members |> match_list [] in
      let new_score = compute_total_score tbl new_match in
      match new_score with
      | 0 -> let _ = Printf.printf "\n Number iterations: %d \n" num_iter in new_match
      | _ ->
          if new_score < best_score then loop (num_iter + 1) new_match new_score
          else loop (num_iter + 1) best_match best_score
  in
  let first_match = shuffle members |> match_list [] in
  loop 1 first_match (compute_total_score tbl first_match)

let write_to_irmin our_match db_path =
  let git_config = Irmin_git.config ~bare:true db_path in
  let yojson_string_to_print =
    Yojson.Safe.to_string (matches_to_yojson { matched = our_match })
  in
  let irmin_write =
    Git_store.Repo.v git_config >>= Git_store.master >>= fun t ->
    Git_store.set_exn t
      [ "matches"; string_of_float (Unix.time ()) ]
      yojson_string_to_print ~info:(info "my first commit")
  in
  Lwt_main.run irmin_write

type case_record = { channel : string; db_path : string }

let write_to_irmin_and_slack our_match case =
  let output = create_output our_match in
  let () = Printf.printf "%s" output in
  let args_message =
    [
      "-F";
      "token=" ^ token;
      "-F";
      "channel=" ^ case.channel;
      "-F";
      "text=" ^ output;
    ]
  in
  match
    Curly.(
      run ~args:args_message
        (Request.make ~url:"https://slack.com/api/chat.postMessage" ~meth:`POST
           ()))
  with
  | Ok _ ->
      write_to_irmin our_match case.db_path 
  | Error e -> Format.printf "Failed babqb: %a" Curly.Error.pp e

let test_case = { channel = test_channel; db_path = "irmin/new" }

let real_case = { channel; db_path = "../irmin/real" }

let blah case =
  write_to_irmin_and_slack
    (calculate_most_optimum_match 100000000
       (construct_hashmap (all_old_matches case.db_path)))
  case 

let () = blah test_case

let _ = Hashtbl.iter (fun (uid1, uid2) score -> Printf.printf "Pair: %s , %s; score: %i\n" uid1 uid2 score) (construct_hashmap (all_old_matches "irmin/new"))
