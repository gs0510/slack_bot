open Yojson.Basic
open Yojson.Basic.Util

let config = from_file "config"

let real_channel = member "channel_id" config |> to_string

let test_channel = member "test_channel_id" config |> to_string

open Types

let test_case =
  { channel = test_channel; db_path = "irmin/new"; num_iter = 1000 }

let real_case =
  { channel = real_channel; db_path = "../irmin/real"; num_iter = 100000000 }

let write_matches_to_irmin_and_slack our_match case =
  let output = Match.to_string our_match in
  let () = Printf.printf "%s" output in
  match Lwt_main.run (Http_requests.write_matches case.channel output) with
  | Ok _ -> Irmin_io.write_matches_to_irmin our_match case.db_path
  | Error e ->
      Format.printf "Http Request to write to slack failed with error : %s" e

let write_opt_in_to_irmin_and_slack case =
  match Lwt_main.run (Http_requests.write_opt_in_message case.channel) with
  | Ok ts -> Irmin_io.write_timestamp_to_irmin ts case.db_path
  | Error e ->
      Format.printf "Http Request to write to slack failed with error : %s" e

let main case =
  write_matches_to_irmin_and_slack (Match.get_most_optimum case) case

let () =
  print_endline test_case.channel;
  main test_case
