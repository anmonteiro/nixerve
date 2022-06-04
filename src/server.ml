open Piaf
open Lwt.Syntax
open Stdune

type ctx =
  { system : System.t
  ; client_addr : Unix.sockaddr
  }

let not_found = Lwt.return (Response.of_string ~body:"Not found" `Not_found)

module Narinfo = struct
  let payload ~hash ~storePath path_info =
    let { Nix.Path_info.narHash
        ; narSize
        ; references
        ; deriver
        ; ca
        ; signatures
        ; _
        }
      =
      path_info
    in
    Format.asprintf
      {|StorePath: %s
URL: nar/%s-%s.nar
Compression: none
NarHash: sha256:%s
NarSize: %d
References: %s
%s%s%s|}
      storePath
      hash
      narHash
      narHash
      narSize
      (String.concat ~sep:" " (List.map ~f:Filename.basename references))
      (Option.value
         ~default:""
         (Option.map
            ~f:(fun deriver ->
              Format.asprintf "Deriver: %s\n" (Filename.basename deriver))
            deriver))
      (Option.value
         ~default:""
         (Option.map ~f:(fun ca -> Format.asprintf "CA: %s\n" ca) ca))
      (if signatures != []
      then Format.asprintf "Sig: %s\n" (String.concat ~sep:" " signatures)
      else "")
end

let cache_info =
  let headers = Headers.of_list [ "Content-Type", "text/plain" ] in
  Lwt.return
    (Response.of_string
       ~headers
       ~body:
         (Format.asprintf
            "StoreDir: %s\nWantMassQuery: 1\nPriority: 30\n"
            Nix.Store.prefix)
       `OK)

let stripExtension filename =
  let segments = String.split_on_char ~sep:'.' filename in
  (* XXX(anmonteiro): doesn't handle x.y.ext *)
  List.hd segments

let narinfo { System.db } target =
  let hashPart = stripExtension (Filename.basename target) in
  let* storePath = Nix.Store.path_by_hash db ~hash:hashPart in
  match storePath with
  | Ok (Some storePath) ->
    let+ path_info = Nix.Path_info.query ~path:storePath db in
    (match path_info with
    | Ok ({ narHash; path; _ } as path_info) ->
      assert (String.length narHash = 52);
      assert (path = storePath);
      let body =
        Narinfo.payload ~storePath ~hash:hashPart path_info
        (* TODO: key file / sigs part *)
      in
      let headers =
        Headers.(of_list [ Well_known.content_type, "text/x-nix-narinfo" ])
      in
      Response.of_string ~body ~headers `OK
    | Error e ->
      Logs.debug (fun m ->
          m "Error getting path info for %s: %a" storePath Caqti_error.pp e);

      assert false)
  | Ok None -> not_found
  | Error e ->
    Logs.debug (fun m -> m "Didn't find path for: %a" Caqti_error.pp e);
    not_found

(* with *)
(* pathInfo as ( *)
(* select id, path, hash, registrationTime, deriver, narSize, sigs, ca from *)
(* ValidPaths where path = *)
(* "/nix/store/dgya41g2wv0c731w5qj8whdmlrsada18-ocaml4.14.0-rc1-uchar-0.0.2" *)
(* ), *)
(* depRefs as ( *)
(* select path as reference from Refs join ValidPaths on reference = id where *)
(* referrer = (select id from pathInfo) *)
(* ) *)

(* SELECT path, *)
(* hash AS narHash, *)
(* narSize, *)
(* deriver, *)
(* registrationTime, *)
(* sigs AS signatures, *)
(* ca, *)
(* r.reference *)
(* FROM pathInfo pi *)
(* LEFT JOIN depRefs r ON true *)
(* UNION ALL *)
(* SELECT path, *)
(* hash AS narHash, *)
(* narSize, *)
(* deriver, *)
(* registrationTime, *)
(* sigs AS signatures, *)
(* ca, *)
(* r.reference *)
(* FROM depRefs r *)
(* LEFT JOIN pathInfo pi ON true *)
(* WHERE r.reference IS NULL; *)

(* SELECT path, *)
(* hash AS narHash, *)
(* narSize, *)
(* deriver, *)
(* registrationTime, *)
(* sigs AS signatures, *)
(* ca, *)
(* r.reference *)
(* FROM  pathInfo pi *)
(* FULL JOIN depRefs r ON true; *)

let stream_of_process ?(env = Unix.environment ()) cmd ~args =
  let command =
    ( cmd
    , Array.init
        ~f:(function 0 -> cmd | n -> args.(n - 1))
        (Array.length args + 1) )
  in
  let process = Lwt_process.open_process_in ~timeout:10. ~env command in
  let ic = process#stdout in
  let stream =
    Lwt_stream.from (fun () ->
        let len = Piaf.Config.default.body_buffer_size in
        let bs = Bigstringaf.create len in
        let* read = Lwt_io.read_into_bigstring ic bs 0 len in
        match read with
        | 0 ->
          let* (Unix.WEXITED _code | WSIGNALED _code | WSTOPPED _code) =
            process#status
          in
          assert (process#state <> Running);
          let+ (Unix.WEXITED _code | WSIGNALED _code | WSTOPPED _code) =
            process#close
          in
          None
        | read -> Lwt.return_some (IOVec.make bs ~off:0 ~len:read))
  in
  Lwt.return stream

let stream_nar { System.db } ?expectedNarHash hash =
  let path_stream_response storePath =
    let headers = Headers.(of_list [ Well_known.content_type, "text/plain" ]) in
    let+ stream =
      stream_of_process "nix" ~args:[| "store"; "dump-path"; "--"; storePath |]
    in
    Response.of_stream ~headers ~body:stream `OK
  in
  let* storePath = Nix.Store.path_by_hash db ~hash in
  match storePath with
  | Ok (Some storePath) ->
    let* ret = Nix.Path_info.query ~path:storePath db in
    (match ret with
    | Ok { Nix.Path_info.narHash; path; _ } ->
      assert (String.length narHash = 52);
      assert (path = storePath);
      (match expectedNarHash with
      | Some expectedNarHash ->
        if not (String.equal expectedNarHash narHash)
        then
          (* return [404, ['Content-Type' => 'text/plain'], ["Incorrect NAR
             hash. Maybe the path has been recreated.\n"]] *)
          not_found
        else path_stream_response storePath
      | None -> path_stream_response storePath)
    | Error e ->
      Logs.debug (fun m -> m "Error getting path info: %a" Caqti_error.pp e);
      not_found)
  | Ok None -> not_found
  | Error e ->
    Logs.debug (fun m -> m "Didn't find path for: %a" Caqti_error.pp e);
    not_found

let nar ctx target =
  let filename = Filename.basename target in
  let expectedNarHash, hash =
    match String.split_on_char ~sep:'-' filename with
    | [ hashPart ] -> None, stripExtension hashPart
    | [ hashPart; expectedNarHash ] ->
      Some (stripExtension expectedNarHash), hashPart
    | _ -> assert false
  in
  stream_nar ctx ?expectedNarHash hash

let handler { Server.request; ctx = { system; _ } } =
  (* let* () = Lwt.pause () in *)
  match request.target with
  | "/nix-cache-info" -> cache_info
  | target when String.ends_with ~suffix:".narinfo" target ->
    (* TODO(anmonteiro): actual regex /^\/([0-9a-z]+)\.narinfo$/ *)
    narinfo system target
  | target when String.ends_with ~suffix:".nar" target ->
    (* TODO(anmonteiro): actual regex /^\/nar\/([0-9a-z]+)-([0-9a-z]+)\.nar$/ *)
    nar system target
  | _ -> assert false

let wrap_context system next request =
  next
    { request with Server.ctx = { system; client_addr = request.Server.ctx } }

let wrap_request_logging next ({ Server.request; _ } as ctx) =
  Logs.debug (fun m -> m "Incoming request: %a" Request.pp_hum request);
  next ctx

let make_handler system = handler |> wrap_request_logging |> wrap_context system

let make_component ~config make_handler =
  let module Component = struct
    type ctx = unit
    type t = Lwt_io.server
    type args = System.t -> (t, [ `Msg of string ]) Lwt_result.t

    let start () ctx =
      let handler = make_handler ctx in
      let listen_address =
        Unix.(ADDR_INET (inet_addr_any, config.System.web_port))
      in
      let+ server =
        Lwt_io.establish_server_with_client_socket
          listen_address
          (Server.create handler)
      in
      Logs.info (fun m ->
          m "Server started, listening on port %d" config.web_port);
      Ok server

    let stop = Lwt_io.shutdown_server
  end
  in
  (module Component : Archi_lwt.Component.COMPONENT
    with type t = Lwt_io.server
     and type args = System.t
                     -> (Lwt_io.server, [ `Msg of string ]) Lwt_result.t
     and type ctx = unit)

let printHash32 hash =
  let base32Chars = "0123456789abcdfghijklmnpqrsvwxyz" in
  let len = 52 in
  let ret = Bytes.create len in
  for n = len - 1 to 0 do
    let b = n * 5 in
    let i = b / 8 in
    let j = b mod 8 in
    let c =
      (Char.code hash.[i] lsr j)
      lor if i >= len - 1 then 0 else Char.code hash.[i + 1] lsl (8 - j)
    in
    Bytes.unsafe_set ret (len - (n + 1)) base32Chars.[c land 0x1f]
  done;
  String.of_bytes ret
