let () = print_endline "Hello, World!"

let _ = Printf.printf "1"
let token = "xoxb-18193414053-1098751527969-2WaxTFvOO9yj4bf9JpGTJ1yD"

let _ = Printf.printf "2"

let session = Slacko.start_session token

let _ = Printf.printf "3"

let channel = Slacko.channel_of_string 
"C01380ZAVGQ"

let _ = Printf.printf "4"

(* let client_secret = "d58aefac32c528902cc9a5bcede7e677"
let client_id = "18193414053.1098751030673"
let code ="c84af629d0f085ca758d71e240c437cc"
let oauth = match (Lwt_main.run (Slacko.oauth_access client_id client_secret code )) with
| `Success obj -> obj
| _ -> failwith "Blah"

let _ = Printf.printf "Oauth: %s %s" oauth.access_token oauth.scope *)

let args = ["-F"; "token=xoxb-18193414053-1098751527969-2WaxTFvOO9yj4bf9JpGTJ1yD"; "-F"; "channel=C01380ZAVGQ"; ]

let members = match (Curly.(run ~args (Request.make ~url:"https://slack.com/api/conversations.members" ~meth:`POST ()))) with
| Ok x ->
  Format.printf "body: %s\n" x.Curly.Response.body
| _ -> failwith "There's an error"

let _ = Printf.printf "5"

(* let user_names = let get_user_name = (fun user -> match (Lwt_main.run (Slacko.users_info session user)) with
| `Success obj -> obj.name 
| _ -> failwith "Can't obtain the user name.") in
List.map get_user_name members

let _ = Printf.printf "%s" (List. hd user_names ) *)

