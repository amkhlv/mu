{
  pkgs ? import <nixpkgs> { },
  ...
}:
pkgs.haskell.lib.overrideCabal (pkgs.haskellPackages.callCabal2nix "mu" ./. { }) (old: {
  postInstall =
    (old.postInstall or "");
})
