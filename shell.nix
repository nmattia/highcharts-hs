{ with-jupyter ? false }:
let
  pkgs = import ./nix {};
  ROOT = builtins.toString ./.;
  ihaskell = pkgs.ihaskellWithPackages (ps: [ ps.highcharts ]);
  functions =
    [
      ''
        hc_ghci() {
          cabal new-repl
        }

        hc_build() {
          cabal new-build
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
    packages = p: [ p.highcharts p.language-javascript-qq ];
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
