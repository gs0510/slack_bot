open Yojson.Basic
open Yojson.Basic.Util

let config = from_file "config"

let token = member "token" config |> to_string

let bot_id = member "bot_id" config |> to_string

let get_members _channel = [ (*gargi*) "UNQPQU9UH"; (*Sonja*) "U010GCFPJH4" ]

(* let args_channel = [ "-F"; "token=" ^ token; "-F"; "channel=" ^ channel ] in
   (match
      Curly.(
        run ~args:args_channel
          (Request.make ~url:"https://slack.com/api/conversations.members"
             ~meth:`POST ()))
    with
   | Ok x ->
       List.map to_string
         (from_string x.Curly.Response.body
         |> Util.member "members" |> Util.to_list)
   | _ -> failwith "There's an error")
   |> List.filter (fun id ->
          id <> bot_id
          (*
          id <> "UNZD1GY4W" (*lyrm*) &&
            id <>  "U0PFW68A3" (*engil*) &&
            id <> "UEDTALRKR" (*Celine*) &&
     		 id <> "U0218CTLAF2" (*Umashankar*) &&
          id <> "UQ7RKM5U7" (*patrick*) &&
          id <> "U0JP4EH7H" (*samoht*) &&
          *)
          (*folks who skip this week*)
          && id <> "UDRKCMFCP"
          (*craigfe*) && id <> "UNQPQU9UH"
          (*gargi*) && id <> "U0J5T0YUD"
          (*gemma-gordon*) && id <> "U023HS3GFPX"
          (*Christine Rose*) && id <> "USAEFBTSS"
          (*ulysse*) && id <> "UHG9PG222"
          (*NathanReb*) && id <> "U01FFLZG0TZ"
          (*ngoguey*) && id <> "U023CTF6A56"
          (*shreyaswikriti*) && id <> "U01M5NDAD8Q"
          (* Gabriel Belouze *) && id <> "ULYMRQKAL"
          (*iona*) && id <> "U023CTFM92L"
          (* dikshagupta*) && id <> "U0XKUH6LB"
          (*trefis*) && id <> "U845EHFPT"
          (*Kate*) && id <> "U0JMF1GRW"
          (*def*) && id <> "U0U6CJGH0"
          (*dinosaure*) && id <> "U016FMK46NR"
          (*Ulugbek*) && id <> "UAP0GA934"
          (* zshipko *)
          (* people on vacation*)
          && id <> "UEQMNGNH0"
          (*pascutto*) && id <> "U022P9TQ76X"
          (* odinaka joy*) && id <> "UFJNZ2ZH9" (*Jules*)
          (* folks skipping until September*)
          && id <> "U0J6HJZ29"
          (* kc*) && id <> "U0J5U03J4"
          (*avsm*) && id <> "U9GE7FGTH"
          (* lortex *) && id <> "U013SFKC15M"
          (* Antonin Décimo *) && id <> "U0JCSR1HT" (* magnus *)
          (*folks who skip permanently*)
          && id <> "U0118JHAUG7"
          (* yman *) && id <> "UU5DVAJQ6"
          (* Romain Liautaud *)) *)

let write_to_slack channel output =
  let args_message =
    [
      "-F"; "token=" ^ token; "-F"; "channel=" ^ channel; "-F"; "text=" ^ output;
    ]
  in
  Curly.(
    run ~args:args_message
      (Request.make ~url:"https://slack.com/api/chat.postMessage" ~meth:`POST ()))

open Lwt.Infix

let req channel output =
  let uri = Uri.of_string "https://slack.com/api/chat.postMessage" in
  let headers =
    Cohttp.Header.of_list
      [
        ("Content-type", "application/json");
        ("Authorization", "Bearer " ^ token);
      ]
  in
  let body =
    `Assoc [ ("channel", `String channel); ("text", `String output) ]
  in
  let serialized_body = Yojson.Basic.to_string body in
  Cohttp_lwt_unix.Client.post ~headers ~body:(`String serialized_body) uri
  >>= fun (rsp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body' ->
  match Cohttp.Code.(code_of_status rsp.status |> is_success) with
  | false -> Error body'
  | true -> (
      print_endline body';
      try Ok (Yojson.Basic.from_string body')
      with Yojson.Json_error err -> Error err)

let parse_reactions_response resp =
  print_endline resp;
  try
    Ok
      (List.map Yojson.Safe.Util.to_string
         (List.map
            Yojson.Safe.Util.(member "users")
            (Yojson.Safe.from_string resp
            |> Yojson.Safe.Util.(member "message")
            |> Yojson.Safe.Util.(member "reactions")
            |> Yojson.Safe.Util.to_list)
         |> Yojson.Safe.Util.flatten))
  with Yojson.Json_error err -> Error err

let get_reactions channel =
  let timestamp = "1636482281.001600" in
  let uri =
    Uri.of_string
      (Format.sprintf
         "https://slack.com/api/reactions.get?channel=%s&timestamp=%s" channel
         timestamp)
  in
  let _ = print_endline (Uri.to_string uri) in
  let headers =
    Cohttp.Header.of_list [ ("Authorization", "Bearer " ^ token) ]
  in
  Cohttp_lwt_unix.Client.get ~headers uri >>= fun (rsp, body) ->
  Cohttp_lwt.Body.to_string body >|= fun body' ->
  match Cohttp.Code.(code_of_status rsp.status |> is_success) with
  | false -> Error body'
  | true ->
      print_endline body';
      parse_reactions_response body'