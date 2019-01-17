{nixpkgs ? import <nixpkgs> {}}:
let
  reflex-course-vm = import ../nix/vm.nix { inherit nixpkgs; };
  jobs = {
    inherit reflex-course-vm;
  };
in
  jobs
