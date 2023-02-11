let pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  name = "plant";
  buildInputs = with pkgs; [
    elmPackages.elm
    elmPackages.elm-format
    # elmPackages.lamdera not supported on macOS
  ];
}
