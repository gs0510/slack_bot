
let token = "xoxb-18193414053-1098751527969-2WaxTFvOO9yj4bf9JpGTJ1yD"


let args_channel = ["-F"; "token=xoxb-18193414053-1098751527969-2WaxTFvOO9yj4bf9JpGTJ1yD"; "-F"; "channel=C011F85NCNT"; ]


open Yojson.Basic
open Yojson.Basic.Util 
let members = (match (Curly.(run ~args:args_channel (Request.make ~url:"https://slack.com/api/conversations.members" ~meth:`POST ()))) with
| Ok x -> List.map to_string (from_string x.Curly.Response.body |> Util.member "members" |> Util.to_list)
| _ -> failwith "There's an error")
|> List.filter (fun id ->  (id <> "U012WN3FHUH"))


let args_user id = ["-F"; "token=xoxb-18193414053-1098751527969-2WaxTFvOO9yj4bf9JpGTJ1yD"; "-F"; (Printf.sprintf "user=%s" id)]
let user_names = let get_name_from_id id = (match (Curly.(run ~args:(args_user id) (Request.make ~url:"https://slack.com/api/users.info" ~meth:`POST ()))) with 
| Ok x ->  from_string x.Curly.Response.body |> Util.member "user" |> Util.member "name" |> to_string
| _ -> failwith "Can't get user name from id") 
in 
(List.map get_name_from_id members)

let random_init = Random.init(int_of_float (Unix.time ()))

let shuffle list =
    let nd = List.map (fun c -> let random = Random.bits () in 
      (random, c)) list in
    let sond = List.sort compare nd in
    List.map snd sond


let rec match1 output users = 
match users with
| [] -> output ^ "\n Have some nice coffee chats :)�"
| [last] -> output ^ (Printf.sprintf " with <@%s> \n Have some nice coffee chats :)" last)
| f::s::tl -> match1 (output ^ (Printf.sprintf "\n <@%s> with <@%s>" f s)) tl 

let _ = Printf.printf "%s" (shuffle members |> match1 "") 

let args_message = ["-F"; "token=xoxb-18193414053-1098751527969-2WaxTFvOO9yj4bf9JpGTJ1yD"; "-F"; "channel=C011F85NCNT"; "-F"; ("text="^(shuffle members |> match1 ""))]

let _ = match (Curly.(run ~args:args_message (Request.make ~url:"https://slack.com/api/chat.postMessage" ~meth:`POST ()))) with
    | Ok _ -> Printf.printf "yay"
    | Error e -> Format.printf "Failed: %a" Curly.Error.pp e
