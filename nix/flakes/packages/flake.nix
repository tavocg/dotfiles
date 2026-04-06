{
  description = "User packages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.default =
      let pkgs = import nixpkgs { system = "x86_64-linux"; };
      in pkgs.buildEnv {
        name = "my-packages";
        paths = with pkgs; [
          git
          lazygit
          htop
          neovim
          gcc
          rnote
        ];
      };
  };
}
