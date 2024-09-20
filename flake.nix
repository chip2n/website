{
  description = "My personal website";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };
  outputs = { self, nixpkgs, ... }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    trivial-file-watch = pkgs.sbcl.buildASDFSystem {
      pname = "trivial-file-watch";
      version = "0.0.1";
      src = pkgs.fetchFromGitHub {
        owner = "chip2n";
        repo = "trivial-file-watch";
        rev = "cdb1bd21bee4944e19a934b97b1c836161878752";
        hash = "sha256-Jgo52cso68Bn5RfWyjUQVsGiICT/80dKoO2n5yJD014=";
      };
      systems = [ "trivial-file-watch" ];
      lispLibs = with pkgs.sbcl.pkgs; [
        bt-semaphore
        cl-inotify
        cl-fad
      ];
    };
    navi = pkgs.sbcl.buildASDFSystem {
      pname = "navi";
      version = "0.0.1";
      src = pkgs.fetchFromGitHub {
        owner = "chip2n";
        repo = "navi";
        rev = "1a625ad1b5d60681d664a814f8ca5c3917d1f9d1";
        hash = "sha256-Fadu4j300kOYAIrTMZJ2CjjdUVPbSN2pI83TjBYO1Rg=";
      };
      systems = [ "navi" ];
      lispLibs = with pkgs.sbcl.pkgs; [
        alexandria
        trivial-file-watch
        clack
        clack-handler-hunchentoot
        lack-middleware-static
        websocket-driver
        spinneret
        lass
        arrows
        cl-org-mode
        cl-ppcre
        cl-fad
        str
      ];
    };
    website = pkgs.sbcl.buildASDFSystem {
      pname = "website";
      version = "0.0.1";
      src = ./.;
      systems = [ "website" ];
      lispLibs = with pkgs.sbcl.pkgs; [
        navi
        spinneret
      ];
    };
    sbcl' = pkgs.sbcl.withOverrides (self: super: {
      inherit trivial-file-watch;
      inherit navi;
      inherit website;
    });
    lisp = sbcl'.withPackages (ps: [ ps.website ]);
    build-site = pkgs.writeScriptBin "build-site" ''
      #!/usr/bin/env bash
      ${lisp}/bin/sbcl --script deploy.lisp
    '';
    run-site = pkgs.writeScriptBin "run-site" ''
      ${lisp}/bin/sbcl --no-userinit \
                       --eval '(require :asdf)' \
                       --eval '(require :website)' \
                       --eval '(site:start)'
    '';
    run-repl = pkgs.writeScriptBin "run-repl" ''
      ${lisp}/bin/sbcl --no-userinit \
                       --eval '(require :asdf)' \
                       --eval '(require :website)'
    '';
  in {
    apps.${system} = {
      default = {
        type = "app";
        program = "${run-site}/bin/run-site";
      };
      deploy = {
        type = "app";
        program = "${build-site}/bin/build-site";
      };
    };
    packages.${system}.default = build-site;
    devShells.${system}.default = pkgs.mkShell {
      shellHook = ''
        ${run-repl}/bin/run-repl
      '';
    };
  };
}
