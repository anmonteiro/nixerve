open Std
open (val Logging.setup __FILE__)
open Archi_lwt

module NixStoreDb = struct
  let component =
    let start () : (Nix.Store.t, [> `Msg of string ]) result Lwt.t =
      let uri = Nix.Store.db_uri in
      match Caqti_lwt.connect_pool ~max_size:10 uri with
      | Ok pool -> Lwt.return_ok pool
      | Error e ->
        let msg = Caqti_error.show e in
        Logs.err (fun m -> m "Failed connecting to %a: %s" Uri.pp_hum uri msg);
        Lwt.return_error (`Msg msg)
    in
    let stop pool = Caqti_lwt.Pool.drain pool in
    Archi_lwt.Component.make ~start ~stop
end

type config =
  { db_uri : Uri.t
  ; web_port : int
  }

let local_config env =
  { db_uri =
      Option.get
        ~default:Nix.Store.db_uri
        (Option.map Uri.of_string (Env.get env "DB_URI"))
  ; web_port =
      int_of_string (Env.get_with_default env ~default:"8080" "WEB_PORT")
  }

let live_config env = local_config env
let test_config env = local_config env

type t = { db : Nix.Store.t }

let make_system _config =
  System.make_reusable ~lift:(fun db -> { db }) [ "db", NixStoreDb.component ]
