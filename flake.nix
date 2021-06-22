{
  description = "Package build for gstreamer project";
  nixConfig.bash-prompt = "\[nix-develop\]$ ";
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
                packages.cuda.components.library.libs = with pkgs; lib.mkForce 
                  [ cudatoolkit_10_2 ];
                # packages.cuda.components.library.pkgconfig = with pkgs; lib.mkForce 
                #  [ [ cudatoolkit_10_2 ] ];
                packages.streamline.components.library.libs = with pkgs; lib.mkForce [ 
                  gst_all_1.gstreamer 
                  gst_all_1.gst-devtools 
                  gst_all_1.gst-plugins-base 
                  gst_all_1.gst-plugins-bad 
                  gst_all_1.gst-plugins-good 
                  gst_all_1.gst-plugins-ugly 
                  gst_all_1.gst-libav
                  gst_all_1.gst-editing-services
                ];
              }];
            };
          # Maps haskell library names to nixpkgs library names
          "gstreamer-1.0" = pkgs.gst_all_1.gstreamer;
          # "gstreamer-devtools-1.0" = pkgs.gst_all_1.gst-devtools;
          # "gstreamer-plugins-base-1.0" = pkgs.gst_all_1.gst-plugins-base;
          # "gstreamer-plugins-bad-1.0" = pkgs.gst_all_1.gst-plugins-bad;
          # "gstreamer-plugins-good-1.0" = pkgs.gst_all_1.gst-plugins-good;
          # "gstreamer-plugins-ugly-1.0" = pkgs.gst_all_1.gst-plugins-ugly;
          # "gst-libav" = pkgs.gst_all_1.gst-libav;
          cuda = prev.cudatoolkit_10_2;
        })
      ];
      pkgs = import nixpkgs { 
          inherit system overlays; 
          config.allowUnfree = true; 
      };
      flake = pkgs.streamlineProject.flake {};
      in flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."streamline:exe:streamline";

        # This is used by `nix develop .` to open a shell for use with
        # `cabal`, `hlint` and `haskell-language-server`
        devShell = pkgs.streamlineProject.shellFor {
          tools = {
            cabal = "latest";
            ghcid = "latest";
            hlint = "latest";
            haskell-language-server = "latest";
          };
        };
        apps.repl = flake-utils.lib.mkApp {
          drv = pkgs.writeShellScriptBin "repl" ''
            confnix=$(mktemp)
            echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
            trap "rm $confnix" EXIT
            nix repl $confnix
          '';
        };
      });
}
