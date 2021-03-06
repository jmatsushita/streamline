{ mkDerivation, base, gi-glib, gi-gst, gstreamer
, gstreamer-plugins-bad, gstreamer-plugins-base, haskell-gi-base
, stdenv, text
}:
mkDerivation {
  pname = "streamline";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base gi-glib gi-gst haskell-gi-base text
  ];
  executablePkgconfigDepends = [
    gstreamer gstreamer-plugins-bad gstreamer-plugins-base
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
