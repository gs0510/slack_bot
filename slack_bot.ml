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
  |> List.filter (fun id -> id <> bot_id && id <>"U0J5U03J4" && id <> "U0JP4EH7H")
  |> List.map (fun member -> "<@" ^ member ^ ">" ) 


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

(* let rec match1 output users =
  match users with
  | [] -> output ^ "\n Have some nice coffee chats :)�"
  | [ last ] ->
      output
      ^ Printf.sprintf " with <@%s> \n Have some nice coffee chats :)" last
  | f :: s :: tl ->
      match1 (output ^ Printf.sprintf "\n <@%s> with <@%s>" f s) tl *)

let rec match_list (output : string list list) (users : string list) :
    string list list =
  match users with
  | [] -> output
  | [ last ] -> (
      match output with
      | [] -> failwith "There's only one person in this channel."
      | fst :: tl -> (last :: fst) :: tl )
  | f :: s :: tl -> match_list ([ f; s ] :: output) tl

(* let message = let matches = match1 [] members in 
List.fold_left (fun current_match  acc-> if List.length current_match = 2 
  then (acc^ (Printf.sprintf "\n <@%s> with <@%s>" (List.hd current_match) (List.nth current_match 1)))
else (acc^ Printf.sprintf "\n <@%s> with <@%s> and <@%s>" (List.hd current_match) (List.nth current_match 1) (List.nth current_match 2) ) ) 
"" matches *)

(* let message = let matches1 = match_list [] members in *)

let create_output (matches_list : string list list) =
  List.fold_left
    (fun acc current_match -> acc ^ String.concat " with " current_match ^ "\n")
    "" matches_list

let _ = Printf.printf "%s" (match_list [] members |> create_output)

module Git_store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

let git_config = Irmin_git.config ~bare:true "irmin/new"

let info message = Irmin_unix.info ~author:"Example" "%s" message

open Lwt.Infix

let irmin_read_keys =
  Git_store.Repo.v git_config >>= Git_store.master >>= fun t ->
  (* Git_store.get t ["matches"; "1590251798"] >|= fun s -> Printf.printf "%s" s *)
  (* Git_store.list t [  "matches"] >|=  List.iter (fun (step, kind) ->
        match kind with
        | `Contents -> Printf.printf "FILE %s\n" step
        | `Node -> Printf.printf "DIR %s\n" step) *)

        (* todo: also handle the case of directories with a log*)
        Git_store.list t [  "matches"] >|=  List.map (fun (step, _) -> step)


let all_epocs = Lwt_main.run irmin_read_keys

let irmin_read_content = Git_store.Repo.v git_config >>= Git_store.master >>= fun t ->
  Lwt_list.map_s (fun epoch ->  Git_store.get t ["matches"; epoch]  ) all_epocs

let all_matches = Lwt_main.run irmin_read_content

let parsing_json all_matches_json = to_list all_matches_json |> List.map to_list

let update_key uid1 uid2 tbl =
  let pair = 
  if uid1 < uid2 then (uid1, uid2) else (uid2, uid1) in
  match Hashtbl.find_opt tbl pair with 
  | Some (num_matches) -> Hashtbl.replace tbl pair (num_matches + 1)
  | None -> Hashtbl.add tbl pair 1

let get_score uid1 uid2 tbl =
  let pair = 
  if uid1 < uid2 then (uid1, uid2) else (uid2, uid1) in
  match Hashtbl.find_opt tbl pair with
| Some (num_matches) -> num_matches
| None -> 0 

(* TODO: make the following two functions somehow reasonable!!!! xD *)
 let construct_hashmap matches =
  let tbl = Hashtbl.create 256 in
  List.iter (fun match_json -> from_string match_json |> member "matched" |> parsing_json |> 
  List.iter (fun current_match -> match List.length current_match with
    | 2 ->  update_key (List.nth current_match 0 |> to_string) (List.nth current_match 1 |> to_string) tbl
    | 3 -> update_key (List.nth current_match 0 |> to_string) (List.nth current_match 1 |> to_string) tbl; 
    update_key (List.nth current_match 1 |> to_string) (List.nth current_match 2 |> to_string) tbl;
    update_key (List.nth current_match 0 |> to_string) (List.nth current_match 2 |> to_string) tbl
    | _ -> failwith "not accounted for!"
  )
) matches; tbl

let compute_total_score tbl matches = 
List.fold_left (fun score current_match -> let pair_score = match List.length current_match with 
  | 2 ->  get_score (List.nth current_match 0 ) (List.nth current_match 1 ) tbl
  | 3 -> get_score (List.nth current_match 0 ) (List.nth current_match 1 ) tbl + 
    get_score (List.nth current_match 1) (List.nth current_match 2 ) tbl +
    get_score (List.nth current_match 0) (List.nth current_match 2) tbl
  | _ -> failwith "not accounted for!"
 in (score + pair_score )) 0 matches


let calculate_most_optimum_match num_times tbl=
 let rec loop num_iter best_match best_score = 
    if num_iter = num_times then best_match
    else 
      let new_match = shuffle members |> match_list [] in
  let new_score = compute_total_score tbl new_match in
  match new_score with
  | 0 -> new_match
  | _ -> if new_score < best_score then loop (num_iter+1) new_match new_score
  else loop (num_iter+1) best_match best_score
in  
  let first_match = shuffle members |> match_list []
 in loop 1 first_match (compute_total_score tbl first_match)



  
(* let _ = Printf.printf "hi"

let tbl = Hashtbl.create 256 
let _ = construct_hashmap all_matches tbl *)

let _ = Hashtbl.iter (fun (uid1, uid2) score -> Printf.printf "Pair: %s , %s; score: %i\n" uid1 uid2 score) (construct_hashmap all_matches)

(* let _ = from_string first_entry |> member "matched" |> to_list |> List.hd |> to_list |> List.hd |> to_string |> Printf.printf "\n\n here: %s\n" *)

(* let current_matches = (match_list [] members) in
 let irmin_json = `O [("matched", current_matches)] *)


(* let write_to_irmin_and_slack our_match = *)

 let yojson_string_to_print =
  Yojson.Safe.to_string (matches_to_yojson { matched = match_list [] (shuffle members) })


let irmin_write =
  Git_store.Repo.v git_config >>= Git_store.master >>= fun t ->
  (* Set a/b/c to "Hello, Irmin!" *)
  Git_store.set_exn t
    [ "matches"; string_of_float (Unix.time ()) ]
    yojson_string_to_print
    (*to_string [ matches_to_yojson { matched = match_list [] members } ] *)
    ~info:(info "my first commit")
  (* >>= fun () -> *)
  (* Get a/b/c *)
  (* Git_store.get t [ "matches" ] >|= fun s -> assert (s = "") *)

let () = Lwt_main.run irmin_write

(* let args_message = ["-F"; "token="^token; "-F"; "channel="^test_channel; "-F"; ("text="^( |> create_output))] *)
(* 
let _ = match (Curly.(run ~args:args_message (Request.make ~url:"https://slack.com/api/chat.postMessage" ~meth:`POST ()))) with
    | Ok _ -> Printf.printf "yay"
    | Error e -> Format.printf "Failed babqb: %a" Curly.Error.pp e *)