{ ... }: {
  flake.nixosModules.packages = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      firefox
      neovim
      git
      lazygit
      gcc
      yazi
      rnote
    ];
  };
}
