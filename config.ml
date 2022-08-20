open Mirage

let name =
  let long_name = "name" in
  let doc =
    Key.Arg.info
      ~docv:"<STRING>"
      ~doc:"The name of the unikernel, used to show successful connections \
            in other instances of conntest."
      [ long_name ]
  in
  Key.(create long_name Arg.(required ~stage:`Run string doc))

let string_list_conv ~sep =
  let serialize = 
    let serialize_list x = Fmt.Dump.list x in
    serialize_list (fun fmt v -> Fmt.pf fmt "%S" v)
  in
  let conv = Cmdliner.Arg.(list ~sep string) in
  let runtime_conv = Fmt.str "(Cmdliner.Arg.(list ~sep:'%c' string))" sep in
  Key.Arg.conv ~conv ~runtime_conv ~serialize

let listens =
  let long_name = "listen" in
  let info_v =
    Key.Arg.info
      ~docv:"<PROTO>:<PORT>"
      ~doc:"Which protocol and port to listen to respectively, separated \
            by ':'. E.g. tcp:1234"
      [ long_name ]
  in
  Key.(create long_name
      Arg.(opt_all ~stage:`Run (string_list_conv ~sep:':') info_v)
  )

let connections =
  let long_name = "connect" in
  let docv = "<URI>" in
  let doc = Printf.sprintf
      "Which other conntest-instance URIs to connect to. Query \
       parameters are used to pass extra options, which includes \
       'monitor-bandwidth'. Supported protocols are 'tcp' and 'udp'. \
       Currently only IP's are supported in URIs hostname section. \
       E.g. tcp://1.2.3.4:1234?monitor-bandwidth"
  in
  let info_v = Key.Arg.info ~doc ~docv [ long_name ] in
  Key.(create long_name Arg.(opt_all ~stage:`Run string info_v))

let keys = [
  key name;
  key listens;
  key connections;
]

let main = main ~keys "Unikernel.Main" (stackv4v6 @-> job)

let stack = generic_stackv4v6 default_network

let () = register "conntest" [ main $ stack ]
