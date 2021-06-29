open Yojson.Basic
open Yojson.Basic.Util

let config = from_file "config"

let token = member "token" config |> to_string

let bot_id = member "bot_id" config |> to_string

let get_members channel =
  let args_channel = [ "-F"; "token=" ^ token; "-F"; "channel=" ^ channel ] in
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
          id <> "U013SFKC15M" (* Antonin Décimo *) &&
         id <> "U01FFLZG0TZ" (*ngoguey*) 
    		 id <> "U0218CTLAF2" (*Umashankar*) &&
          id <> "U0U6CJGH0" (*dinosaure*) &&
         id <> "UDRKCMFCP" (*craigfe*) &&
         id <> "UHG9PG222" (*NathanReb*) &&
         *)
         (*folks who skip this week*)
         && id <> "UNQPQU9UH"
         (*gargi*) && id <> "U016FMK46NR"
         (*Ulugbek*) && id <> "UQ7RKM5U7"
         (*patrick*) && id <> "U9GE7FGTH"
         (* lortex *) && id <> "U023HS3GFPX"
         (*Christine Rose*) && id <> "ULYMRQKAL"
         (*iona*) && id <> "U0XKUH6LB"
         (*trefis*) && id <> "U0J5T0YUD"
         (*gemma-gordon*) && id <> "U0JP4EH7H"
         (*samoht*) && id <> "U0J5U03J4"
         (*avsm*) && id <> "UFJNZ2ZH9"
         (*Jules*) && id <> "U01M5NDAD8Q"
         (* Gabriel Belouze *) && id <> "U0JMF1GRW"
         (*def*) && id <> "U0JCSR1HT"
         (* magnus *) && id <> "USAEFBTSS"
         (*ulysse*) && id <> "UAP0GA934"
         (* zshipko *) && id <> "UEQMNGNH0"
         (*pascutto*) && id <> "U023CTFM92L"
         (* dikshagupta*) && id <> "U023CTF6A56"
         (*shreyaswikriti*) && id <> "U022P9TQ76X"
         (* odinaka joy*) && id <> "U845EHFPT"
         (*Kate*) && id <> "U0J6HJZ29"
         (* kc*)
         (*folks who skip permanently*)
         && id <> "U0118JHAUG7"
         (* yman *) && id <> "UU5DVAJQ6"
         (* Romain Liautaud *))

let write_to_slack channel output =
  let args_message =
    [
      "-F"; "token=" ^ token; "-F"; "channel=" ^ channel; "-F"; "text=" ^ output;
    ]
  in
  Curly.(
    run ~args:args_message
      (Request.make ~url:"https://slack.com/api/chat.postMessage" ~meth:`POST ()))
