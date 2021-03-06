{
  description = "Package build for accelerate-examples";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, haskellNix, nixpkgs, flake-utils }: 
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          streamlineProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8104";
              # projectFileName = "stack.yaml";
              modules = [{
              # Replace `extra-libraries` dependencies
                packages.cuda.components.library.libs = pkgs.lib.mkForce 
                  [ pkgs.cudatoolkit_10_2 ];
                packages.cuda.components.library.pkgconfig = pkgs.lib.mkForce 
                 [ [ pkgs.cudatoolkit_10_2 ] ];
              }];
            };
          llvm-config = prev.llvm_9;
          cuda = prev.cudatoolkit_10_2;
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; config.allowUnfree = true; };
      flake = pkgs.streamlineProject.flake {};
      in flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."accelerate-examples:exe:accelerate-nbody";

        # This is used by `nix develop .` to open a shell for use with
        # `cabal`, `hlint` and `haskell-language-server`
        devShell = pkgs.streamlineProject.shellFor {
          tools = {
            cabal = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
        };
      });
}
