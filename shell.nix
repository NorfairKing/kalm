let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
in
pkgs.haskell.lib.buildStackProject {
  name = "kalm-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    zlib
  ];
}
