(* Time at which the opt-in question should be sent in UTC
  (Notice that the opt-in message is always sent on Mondays) *)
let opt_int_time = (10, 00, 0)

let weekday_to_int = function
| `Mon -> 0
| `Tue -> 1
| `Wed -> 2
| `Thu -> 3
| `Fri -> 4
| `Sat -> 5
| `Sun -> 6

let time_to_secs (h, min, sec) = sec + min * 60 + h * 60 * 60

let nsecs_of_secs s = Int64.(mul (of_int s) 1_000_000_000L)

let time_since_last_opt_in now =
  let weekday = Ptime.weekday now in
  let _current_date, (current_time, _) = Ptime.to_date_time now in
  let secs_since_eleven = time_to_secs current_time - time_to_secs opt_int_time in
  if weekday = `Mon && secs_since_eleven < 0 then
    - secs_since_eleven
  else
    let secs_since_last_monday = 60 * 60 * 24 *  (weekday_to_int weekday) in
    let secs_in_week = 60 * 60 * 24 * 7 in
    secs_in_week - secs_since_last_monday - secs_since_eleven

let sleep_till_next_opt_in () =
  Time.sleep_ns (nsecs_of_secs (time_since_last_opt_in (Pclock.now_d_ps () |> Ptime.v)))
