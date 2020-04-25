let correspondences =
  [
    ("@trefis", "U0XKUH6LB");
    ("@gargi", "UNQPQU9UH");
    ("@Romain Liautaud", "UU5DVAJQ6");
    ("@gemma-gordon", "U0J5T0YUD");
    ("@avsm", "U0J5U03J4");
    ("@Kate", "U845EHFPT");
    ("@Sonja", "U010GCFPJH4");
    ("@pascutto", "UEQMNGNH0");
    ("@ioana", "UEQMNGNH0");
    ("@magnus", "U0JCSR1HT");
    ("@NathanReb", "UHG9PG222");
    ("@engil", "U0PFW68A3");
    ("@Jules", "UFJNZ2ZH9");
    ("@Céline", "UEDTALRKR");
    ("@yman", "U0118JHAUG7");
    ("@samoht", "U0JP4EH7H");
    ("@def", "U0JMF1GRW");
    ("@ulysse", "USAEFBTSS");
  ]

let member_tbl = Hashtbl.create 20

let _ =
  List.iter
    (fun (uname, uid) -> Hashtbl.add member_tbl uname uid)
    correspondences

let first_matches_names =
  [
    [ "@Céline"; "@ioana" ];
    [ "@magnus"; "@NathanReb" ];
    [ "@Jules"; "@Romain Liautaud" ];
    [ "@gargi"; "@Sonja" ];
    [ "@trefis"; "@pascutto" ];
    [ "@gemma-gordon"; "@yman" ];
    [ "@avsm"; "@ulysse" ];
    [ "@Romain Liautaud"; "@gemma-gordon" ];
    [ "@gargi"; "@samoht" ];
    [ "@avsm"; "@trefis" ];
    [ "@Céline"; "@NathanReb" ];
    [ "@magnus"; "@Jules" ];
    [ "@Sonja"; "@def" ];
    [ "@engil"; "@ulysse" ];
    [ "@Jules"; "@Sonja" ];
    [ "@Romain Liautaud"; "@ulysse" ];
    [ "@trefis"; "@NathanReb" ];
    [ "@gargi"; "@def" ];
    [ "@gemma-gordon"; "@Kate" ];
    [ "@magnus"; "@ioana" ];
    [ "@yman"; "@samoht" ];
    [ "@trefis"; "@gargi" ];
    [ "@Romain Liautaud"; "@gemma-gordon" ];
    [ "@avsm"; "@Kate" ];
    [ "@Sonja"; "@pascutto" ];
    [ "@ioana"; "@magnus" ];
    [ "@NathanReb"; "@engil" ];
    [ "@Jules"; "@Céline" ];
    [ "@yman"; "@samoht" ];
    [ "@def"; "@ulysse" ];
    [ "@def"; "@samoht"; "@engil" ];
    [ "@yman"; "@ioana"; "@pascutto" ];
    [ "@engil"; "@Céline"; "@pascutto" ];
  ]

let first_matches_uids =
  List.map
    (List.map (fun member ->
         match Hashtbl.find_opt member_tbl member with
         | None ->
             let () = Printf.printf "%s\n" member in
             failwith "error"
         | Some uid -> uid))
    first_matches_names

type matches = { matched : string list list } [@@deriving yojson]

module Git_store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

let git_config = Irmin_git.config ~bare:true "irmin/real"

let info message = Irmin_unix.info ~author:"Gargi and Sonja" "%s" message

open Lwt.Infix

let write_to_irmin () =
  let yojson_string_to_print =
    Yojson.Safe.to_string (matches_to_yojson { matched = first_matches_uids })
  in
  let irmin_write =
    Git_store.Repo.v git_config >>= Git_store.master >>= fun t ->
    Git_store.set_exn t
      [ "matches"; string_of_float (Unix.time ()) ]
      yojson_string_to_print ~info:(info "first 4 matches")
  in
  Lwt_main.run irmin_write

let () = write_to_irmin ()
