open Lwt.Syntax

module Hash = struct
  type hash =
    | Base16
    | Base32
    | Base64
    | SRI

  (* Direct port of this code:
   * https://github.com/NixOS/nix/blob/master/src/libutil/hash.cc#L87-L107 *)
  let printHash32 hash =
    let base32Chars = "0123456789abcdfghijklmnpqrsvwxyz" in
    let hashSize = 32 in
    let hash32len = 52 in
    let ret = Bytes.create hash32len in
    for n = hash32len - 1 downto 0 do
      let b = n * 5 in
      let i = b / 8 in
      let j = b mod 8 in
      let c =
        (Char.code hash.[i] lsr j)
        lor if i >= hashSize - 1 then 0 else Char.code hash.[i + 1] lsl (8 - j)
      in
      Bytes.unsafe_set ret (hash32len - (n + 1)) base32Chars.[c land 0x1f]
    done;
    Bytes.unsafe_to_string ret
end

module Store = struct
  let prefix = "/nix/store"
  let store_db = "/nix/var/nix/db/db.sqlite"
  let db_uri = Uri.of_string (Format.asprintf "sqlite3://%s" store_db)

  type t =
    ( Caqti_lwt.connection
    , [ Caqti_error.connect | Caqti_error.transact ] )
    Caqti_lwt.Pool.t

  let query ~f pool =
    let tx ~f connection =
      let (module C : Caqti_lwt.CONNECTION) = connection in
      let* migration_result = f connection in
      match migration_result with
      | Ok result -> Lwt.return_ok result
      | Error error ->
        (match error with
        | #Caqti_error.transact -> Lwt_result.fail error
        | #Caqti_error.connect -> Lwt_result.fail error)
    in
    Caqti_lwt.Pool.use (tx ~f) pool

  let path_by_hash =
    [%rapper
      get_opt
        {sql| SELECT @string{path} FROM ValidPaths WHERE path >= %string{hash} limit 1 |sql}]

  let path_by_hash db ~hash =
    let open Lwt_result.Syntax in
    let pathPrefix = Filename.concat prefix hash in
    let+ ret = query ~f:(path_by_hash ~hash:pathPrefix) db in
    Option.bind ret (fun path ->
        if String.starts_with ~prefix:pathPrefix path then Some path else None)
end

module Path_info = struct
  type t =
    { path : string
    ; narHash : string
    ; narSize : int
    ; references : string list
    ; deriver : string option
    ; registrationTime : int
    ; signatures : string list
    ; ca : string option
    }

  let make_path_info
      ~path
      ~narHash
      ~narSize
      ~deriver
      ~registrationTime
      ~signatures
      ~ca
    =
    { path
    ; narHash
    ; narSize
    ; references = []
    ; deriver
    ; registrationTime
    ; signatures =
        Option.value ~default:[] (Option.map (fun s -> [ s ]) signatures)
    ; ca
    }

  let queryPathInfo ~path c =
    let open Lwt_result.Syntax in
    let f =
      [%rapper
        get_many
          {sql|
with
pathInfo as (
  select id, path, hash, registrationTime, deriver, narSize, sigs, ca from ValidPaths where path = %string{path}
    ),
depRefs as (
  select path as reference from Refs join ValidPaths on reference = id where referrer = (select id from pathInfo)
)

SELECT @string{path},
       hash AS @string{narHash},
       @int{narSize},
       @string?{deriver},
       @int{registrationTime},
       sigs AS @string?{signatures},
       @string?{ca},
       @string?{r.reference}
FROM  pathInfo pi
LEFT JOIN depRefs r ON true
UNION ALL
SELECT path,
hash AS narHash,
narSize,
deriver,
registrationTime,
sigs AS signatures,
ca,
r.reference
FROM depRefs r
LEFT JOIN pathInfo pi ON true
WHERE r.reference IS NULL
|sql}
          function_out]
        (make_path_info, fun ~reference -> reference)
    in

    let+ ret = f ~path c in
    Rapper.load_many
      (fst, fun { path; _ } -> path)
      [ ( snd
        , fun pathInfo references ->
            { pathInfo with
              references = List.filter_map ~f:(fun x -> x) references
            } )
      ]
      ret

  let query ?(hash = Hash.Base32) ~path db =
    let open Lwt_result.Syntax in
    let+ ret = Store.query ~f:(queryPathInfo ~path) db in
    match ret with
    | [ ret ] ->
      let { narHash; _ } = ret in
      let _sha256, hex =
        match String.split_on_char ~sep:':' narHash with
        | [ sha256; hex ] -> sha256, hex
        | _ -> assert false
      in
      let binary = Hex.to_string (`Hex hex) in
      let newNarHash =
        match hash with
        | SRI -> assert false (* not sure what this format is *)
        | Base16 -> hex
        | Base32 -> Hash.printHash32 binary
        | Base64 -> Base64.encode_exn binary
      in
      { ret with narHash = newNarHash }
    | [] -> assert false
    | _ ->
      Logs.err (fun m -> m "p: %s" path);
      assert false
end
