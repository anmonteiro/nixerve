let handle_signal wakener system i =
  let open Lwt.Syntax in
  Logs.info (fun m ->
      m
        "SIG(%s) received (%d), tearing down."
        "SIGHUP"
        i);
  Lwt.async (fun () ->
      let+ _system = Archi_lwt.System.stop system in
      Lwt.wakeup_later wakener ())

let server ?(log_level = Logs.Debug) system =
  let open Lwt.Syntax in
  Printexc.record_backtrace true;
  Sys.(set_signal sigpipe Signal_ignore);
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some log_level);
  let promise =
    let* system = Archi_lwt.System.start () system in
    match system with
    | Ok system ->
      let waiter, wakener = Lwt.wait () in
      let signal_handler = handle_signal wakener system in
      Sys.(set_signal sigint (Signal_handle signal_handler));
      Sys.(set_signal sigterm (Signal_handle signal_handler));
      waiter
    | Error `Cycle_found ->
      Logs.err (fun m -> m "ERROR: Cycle found");
      exit 1
    | Error (`Msg error) ->
      Logs.err (fun m -> m "ERROR: %s" error);
      exit 1
  in
  Lwt_main.run promise