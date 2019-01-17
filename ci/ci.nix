{nixpkgs ? import <nixpkgs> {}}:
let
  reflex-course-vm = import ../nix/vm.nix {};
  jobs = {
    inherit reflex-course-vm;
  };
in
  jobs
