open Yojson.Basic
open Yojson.Basic.Util

let config = from_file "config"

let real_channel = member "channel_id" config |> to_string

let test_channel = member "test_channel_id" config |> to_string

type case_record = { channel : string; db_path : string; num_iter : int }

let test_case =
  { channel = test_channel; db_path = "irmin/new"; num_iter = 1000 }

let real_case =
  { channel = real_channel; db_path = "../irmin/real"; num_iter = 100000000 }

let write_to_irmin_and_slack our_match case =
  let output = Match.to_string our_match in
  let () = Printf.printf "%s" output in
  match Curl_requests.write_to_slack case.channel output with
  | Ok _ -> Irmin_io.write_to_irmin our_match case.db_path
  | Error e ->
      Format.printf "Curl Request to write to slack failed with error : %a"
        Curly.Error.pp e

let main case =
  write_to_irmin_and_slack
    (Match.get_most_optimum case.num_iter case.db_path)
    case

let () = main test_case
