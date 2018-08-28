{ with-jupyter ? true }:
let
  pkgs = import ./nix {};
  ROOT = builtins.toString ./.;
  ihaskell = pkgs.ihaskellWithPackages (ps: [ ps.highcharts ]);
  functions =
    [
      ''
        hc_ghci() {
          pushd ${ROOT}/highcharts
          cabal new-repl
          popd
        }

        hc_build() {
          pushd ${ROOT}/highcharts
          cabal new-build
          popd
        }
      ''
    ] ++ pkgs.lib.optional with-jupyter
      ''
        hc_notebook() {
          ihaskell-notebook
        }

        hc_readme_gen() {
          jupyter-nbconvert \
            --to markdown \
            ${ROOT}/README.ipynb \
            --stdout \
            > README.md
        }
      '';
in pkgs.haskellPackages.shellFor
  {
    packages = p: [ p.highcharts ];
    withHoogle = false;
    buildInputs =
      [ pkgs.cabal-install ] ++ pkgs.lib.optional with-jupyter ihaskell ;
    shellHook =
      pkgs.lib.strings.concatStringsSep "\n"
      (
      [''local pre_functions=$(declare -F | cut -d" " -f3-)'']
      ++ functions ++
      [''
        local post_functions=$(declare -F | cut -d" " -f3-)

        echo "helpers:"
        diff <(echo "$pre_functions") <(echo "$post_functions") | grep '> '

      '']
      );
  }
