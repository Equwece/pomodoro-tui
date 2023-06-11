{ compiler ? "ghc902" }:
let
  stableNixpkgs = import ./stableNixpkgs.nix;
  myNixPkgs = (import stableNixpkgs) {
    overlays = [myNixPkgsOverlay];
  };


  myNixPkgsOverlay = (nixSelf: nixSuper: {
    myHaskellPackages = nixSelf.haskell.packages."${compiler}".override (oldHaskellPkgs: {
      overrides = haskellPackagesNew: oldHaskellPkgs: rec {
        myProject = haskellPackagesNew.callCabal2nix "pomodoro-tui" ./. {};
      };
    });
  });
 
  myDevTools = with myNixPkgs; [
    cabal-install 
    haskell-language-server
    cabal2nix
  ];

  myShellHook = ''
    alias repl="cabal new-repl"
  '';
in
myNixPkgs.myHaskellPackages.myProject.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ myDevTools;
  shellHook = myShellHook;
})
