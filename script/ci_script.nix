{ ghc, stdenv }:

stdenv.mkDerivation ({
  name = "ci_script";
  src = ./.;
  buildInputs = [ ghc ];
  buildPhase = ''
    mkdir -p $out/bin
    ghc Main.hs -o $out/bin/ci_script
  '';
})
