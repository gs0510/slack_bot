open Yojson.Basic
open Yojson.Basic.Util

let config = from_file "config"

let token = member "token" config |> to_string

let channel = member "channel_id" config |> to_string

let bot_id = member "bot_id" config |> to_string

let args_channel = [ "-F"; "token=" ^ token; "-F"; "channel=" ^ channel ]

open Yojson.Basic
open Yojson.Basic.Util

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
  |> List.filter (fun id -> id <> bot_id)

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

let rec match1 (output: string list list)  (users: string list) : string list list =
  match users with
  | [] -> output
  | [ last ] -> (match output with
    | [] -> failwith "There's only one person in this channel."
    | fst :: tl -> ((last :: fst)::tl ))
  | f :: s :: tl -> match1 ([f;s] :: output) tl 

let message = let matches = match1 [] members in 
List.fold_left (fun current_match  acc-> if List.length current_match = 2 
  then (acc^ (Printf.sprintf "\n <@%s> with <@%s>" (List.hd current_match) (List.nth current_match 1)))
else (acc^ Printf.sprintf "\n <@%s> with <@%s> and <@%s>" (List.hd current_match) (List.nth current_match 1) (List.nth current_match 2) ) ) 
"" matches

let _ = Printf.printf "%s" (shuffle members |> match1 "")

(* let args_message = ["-F"; "token="^token; "-F"; "channel="^channel; "-F"; ("text="^(shuffle members |> match1 ""))]

let _ = match (Curly.(run ~args:args_message (Request.make ~url:"https://slack.com/api/chat.postMessage" ~meth:`POST ()))) with
    | Ok _ -> Printf.printf "yay"
    | Error e -> Format.printf "Failed: %a" Curly.Error.pp e *)

module Git_store = Irmin_unix.Git.FS.KV (Irmin.Contents.Json)

let git_config = Irmin_git.config ~bare:true "/tmp/irmin/new"

let info message = Irmin_unix.info ~author:"Example" "%s" message

open Lwt.Infix

let main =
  Git_store.Repo.v git_config >>= Git_store.master >>= fun t ->
  (* Set a/b/c to "Hello, Irmin!" *)
  Git_store.set_exn t [ "Gargi" ]
    [ ("Sonja", `Float 1.0); ("Nathan", `Float 1.0) ]
    ~info:(info "my first commit")
  >>= fun () ->
  (* Get a/b/c *)
  Git_store.get t [ "Gargi" ] >|= fun s ->
  assert (s = [ ("Sonja", `Float 1.0); ("Nathan", `Float 1.0) ])

let () = Lwt_main.run main

let weights =
  let tbl = Hashtbl.create 17 in
  List.iter
    (fun member ->
      let main =
        Git_store.Repo.v git_config >>= Git_store.master >>= fun t ->
        Git_store.get t [ member ] >|= fun former_matches ->
        List.iter (fun other -> Hashtbl.add tbl member other) former_matches
      in
      Lwt_main.run main)
    members

(* let total_weight ran_list mm *)
