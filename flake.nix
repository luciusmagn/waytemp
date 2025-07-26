{
  description = "waytemp - Wayland color temperature control daemon";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        sbcl' = pkgs.sbcl.withPackages (ps: with ps; [
          cffi
          clingon
          iolib
          alexandria
          serapeum
          bordeaux-threads
        ]);

        waytemp-core = pkgs.stdenv.mkDerivation {
          pname = "waytemp-core";
          version = "1.0.0";
          src = ./c;

          nativeBuildInputs = with pkgs; [
            pkg-config
            wayland-scanner
          ];

          buildInputs = with pkgs; [
            wayland
            wayland-protocols
          ];

          buildPhase = "make";

          installPhase = ''
            mkdir -p $out/lib
            cp libwaytemp_core.so $out/lib/
          '';
        };

      in {
        packages = {
          default = self.packages.${system}.waytemp;

          waytemp = pkgs.stdenv.mkDerivation {
            pname = "waytemp";
            version = "1.0.0";
            src = ./.;

            nativeBuildInputs = [ sbcl' ];
            buildInputs = [ waytemp-core ];

            buildPhase = ''
              export HOME=$TMPDIR
              mkdir -p c/
              cp ${waytemp-core}/lib/libwaytemp_core.so c/

              sbcl --eval "(declaim (optimize (speed 3) (safety 3) (debug 3)))" \
                   --eval "(load (sb-ext:posix-getenv \"ASDF\"))" \
                   --eval "(push \"$PWD/\" asdf:*central-registry*)" \
                   --eval "(asdf:make :waytemp)" \
                   --quit
            '';

            installPhase = ''
              mkdir -p $out/bin $out/lib
              cp waytemp $out/bin/
              cp ${waytemp-core}/lib/libwaytemp_core.so $out/lib/
            '';
          };

          # patched libraries
          waytemp-portable = pkgs.stdenv.mkDerivation {
            pname = "waytemp-distrib";
            version = "1.0.0";
            src = self.packages.${system}.waytemp;

            nativeBuildInputs = [ pkgs.patchelf ];

            dontUnpack = true;
            dontBuild = true;

            installPhase = ''
              mkdir -p $out/bin $out/lib

              # Copy files from the base package
              cp $src/bin/waytemp $out/bin/
              cp $src/lib/libwaytemp_core.so $out/lib/

              # Make files writable so patchelf can modify them
              chmod +w $out/bin/waytemp $out/lib/libwaytemp_core.so

              # Patch binary to use system libraries
              patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 $out/bin/waytemp
              patchelf --set-rpath '/usr/lib:/usr/lib64:/lib:/lib64:$ORIGIN/../lib' $out/bin/waytemp

              # Patch the shared library too
              patchelf --set-rpath '/usr/lib:/usr/lib64:/lib:/lib64' $out/lib/libwaytemp_core.so
            '';
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sbcl'
            pkg-config
            wayland-dev
            wayland-protocols
            wayland-scanner
          ];
        };
      });
}
