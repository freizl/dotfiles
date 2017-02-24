{ config, pkgs, ... }:

{

  system.autoUpgrade.enable = true;

  networking.networkmanager.enable = true;

  virtualisation.virtualbox.guest.enable = true;

  environment.systemPackages = with pkgs;
  [ vim
    emacs
    nodejs
    firefox
    git
    rxvt_unicode
    dmenu
    gmrun
    xorg.xmodmap
    xorg.xev
    haskellPackages.xmobar
  ];

  fileSystems = [
    { mountPoint = "/";
      device = "/dev/disk/by-label/nixos";
      label = "nixos";
    }
  ];

  boot = {
    initrd = {
      # Disable journaling check on boot because virtualbox doesn't need it
      checkJournalingFS = false;
      # Make it pretty
      kernelModules = [ "fbcon" ];
    };
    # Use the GRUB 2 boot loader.
    loader.grub = {
      enable = true;
      device = "/dev/sda";
    };
  };

  services =
  {
    xserver =
    {
      enable = true;
      layout = "us";

      displayManager =
      {
        kdm.enable = true;
      };

      desktopManager =
      {
        default = "none";
      };

      windowManager.xmonad =
      {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ];
      };
      windowManager.default = "xmonad";
      displayManager.sessionCommands = with pkgs; lib.mkAfter
      ''
      setxkbmap dvorak
      emacs --daemon &
      urxvtd &
      '';

    };
  };

  users.extraUsers.alice =
  { isNormalUser = true;
    home = "/home/hw";
    description = "Haisheng Wu";
    extraGroups = [ "wheel" "users" "vboxsf" ];
  };
}
