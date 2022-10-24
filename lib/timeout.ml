open Lwt.Infix

type t = {
  timeout_ns : Int64.t;
  progress : unit Lwt_mvar.t;
  timeout : unit Lwt.t;
}

let make ~sleep_ns ~timeout_ns =
  let progress = Lwt_mvar.create () in
  let rec return_on_timeout () =
    Lwt.pick [
      (Lwt_mvar.take progress >|= fun () -> `Progress);
      (sleep_ns timeout_ns >|= fun () -> `Timeout);
    ]
    >>= function
    | `Progress -> return_on_timeout ()
    | `Timeout -> Lwt.return_unit
  in
  let timeout = return_on_timeout () in
  { timeout_ns; progress; timeout }

let progress : t -> unit Lwt.t = fun state ->
  Lwt_mvar.put state.progress ()

let cancel_on_timeout : t -> 'a Lwt.t -> unit =
  fun state t ->
  Lwt.async (fun () -> state.timeout >|= fun () -> Lwt.cancel t)

let on_timeout : t -> (unit -> unit Lwt.t) -> unit Lwt.t =
  fun state f ->
  state.timeout >>= f
