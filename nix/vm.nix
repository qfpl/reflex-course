{nixpkgs ? <nixpkgs> }:
let
  pkgs = import nixpkgs {};
  course-vm-config = {
    imports = [ "${nixpkgs}/nixos/modules/virtualisation/virtualbox-image.nix" ];
 
      virtualbox = {
        baseImageSize = 15 * 1024;
        memorySize = 3 * 1024;
        vmName = "Reflex course (NixOS)";
        vmDerivationName = "nixos-ova-reflex-course";
        vmFileName = "reflex-course.ova";
      };

      services.xserver = {
        enable = true;
        displayManager.sddm = {
          enable = true;
          autoLogin = {
            enable = true;
            relogin = true;
            user = "workshop";
          };
        };
        desktopManager.xfce.enable = true;
        libinput.enable = true; # for touchpad support on many laptops
      };

      users.extraUsers.workshop = {
        isNormalUser = true;
        description = "Workshop user";
        extraGroups = [ "wheel" ];
        password = "workshop";
        uid = 1000;
      };
    
  };
 
  hydraJob = (import "${nixpkgs}/lib/").hydraJob;
  course-vm = hydraJob ((import "${nixpkgs}/nixos/lib/eval-config.nix" {
    modules = [course-vm-config];
  }).config.system.build.virtualBoxOVA);
in
  course-vm
