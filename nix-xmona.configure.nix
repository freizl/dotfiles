{ config, pkgs, ... }:

{
  imports = [ <nixpkgs/nixos/modules/virtualisation/virtualbox-image.nix> ];
  
  system.autoUpgrade.enable = true;

  networking.networkmanager.enable = true;

  environment.systemPackages =
  [ pkgs.vim
    pkgs.emacs
    pkgs.nodejs
    pkgs.firefox
    pkgs.git
  ];

  services.xserver.enable = true;
  services.xserver.displayManager.kdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  users.extraUsers.alice =
  { isNormalUser = true;
    home = "/home/hw";
    description = "Haisheng Wu";
    extraGroups = [ "wheel" "users" "vboxsf" ];
  };
}

