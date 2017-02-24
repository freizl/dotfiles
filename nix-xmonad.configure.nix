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
    #xmodmap
    #gmrun
    #xev
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
      #device = "/dev/disk/by-label/nixos";
      device = "/dev/sda";
    };
  };

  services =
  {
    xserver =
    {
      enable = true;
      layout = "us";

      # displayManager =
      # {
      #   kdm.enable = true;
      # };
      #
      # desktopManager =
      # {
      #   gnome3.enable = true;
      #   default = "gnome3";
      # };

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
    };
  };

  users.extraUsers.alice =
  { isNormalUser = true;
    home = "/home/hw";
    description = "Haisheng Wu";
    extraGroups = [ "wheel" "users" "vboxsf" ];
  };
}
