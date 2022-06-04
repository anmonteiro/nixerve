{ lib, nix-filter, ocamlPackages }:

{
  pname = "nixerve";

  src = nix-filter {
    root = ../.;
    # If no include is passed, it will include all the paths.
    include = [
      (nix-filter.inDirectory ./src)
      (nix-filter.inDirectory ./ocaml-nix)
      (nix-filter.matchExt "opam")
      ./dune-project
    ];
  };

  propagatedBuildInputs = with ocamlPackages; [
    base64
    hex
    piaf
    logs
    fmt
    caqti-lwt
    caqti-driver-sqlite3
    ppx_rapper_lwt
    ppx_rapper
    stdune
    archi
    archi-lwt
  ];
}
