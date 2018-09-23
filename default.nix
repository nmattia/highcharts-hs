let
  pkgs = import ./nix {};
in
  {
    highcharts-types = pkgs.haskellPackages.highcharts-types;
    highcharts = pkgs.haskellPackages.highcharts;
    highcharts-types-src = pkgs.highcharts-types-src;
  }
