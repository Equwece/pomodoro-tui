let
  stableNixpkgs = import ./stableNixpkgs.nix;
  nixpkgs = import (stableNixpkgs) {};
in
  nixpkgs.haskellPackages.callCabal2nix "pomodoro-tui" ./. {}
