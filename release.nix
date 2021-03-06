let
  niv = import nix/sources.nix { };
  pkgs = import niv.nixpkgs { config = {allowUnfree = true;}; };
in
  { 
    streamline = pkgs.haskellPackages.callPackage ./streamline.nix { 
      gstreamer = pkgs.gst_all_1.gstreamer;
      gstreamer-plugins-bad = pkgs.gst_all_1.gst-plugins-bad;
      gstreamer-plugins-base = pkgs.gst_all_1.gst-plugins-base;
      # gstreamer-plugins-good = pkgs.gst_all_1.gst-plugins-good;
    };

    pkg-config = pkgs.pkg-config;
    gstreamer-plugins-ugly = pkgs.gst_all_1.gst-plugins-ugly;
    gstreamer-libav = pkgs.gst_all_1.gst-libav;
    # ffmpeg-full = pkgs.ffmpeg-full;
    gstreamer-devtools = pkgs.gst_all_1.gst-devtools; 
    libav = pkgs.libav;
  }