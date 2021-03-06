let
  release = import ./release.nix;
  shell = release.streamline.env.overrideAttrs (
      old: {
        shellHook = ''
          echo "Hello shell"
        '';
      }
  );
in 
  shell
