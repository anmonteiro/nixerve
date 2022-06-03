open Std
open (val Logging.setup __FILE__)

module WebSystem = struct
  let system =
    let config = System.live_config Env.env in
    let core_system =
      let local_system = System.(make_system config) in
      Archi_lwt.Component.of_system local_system
    in
    Archi_lwt.System.make
      [ "core system", core_system
      ; ( "webserver"
        , Archi_lwt.Component.using_m
            (Server.make_component ~config Server.make_handler)
            ~dependencies:[ core_system ] )
      ]
end

let () = Entrypoint.server ~log_level:Info WebSystem.system
