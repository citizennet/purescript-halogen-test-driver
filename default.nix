{ self, ... }: {
  perSystem = { config, self', inputs', system, pkgs, ... }:

    let
      dependencies = with config.purs-nix.ps-pkgs; [
        halogen
        halogen-vdom
        self'.packages.pre
      ];

      ps = config.purs-nix.purs {
        inherit dependencies;
        dir = ./.;
      };

    in

    {
      packages = {
        halogen-test-driver =
          config.purs-nix.build {
            name = "halogen-test-driver";
            src.path = ./.;
            info = { inherit dependencies; };
          };
      };

      extra-shell-tools = [ (config.make-command { inherit ps; pkg = ./.; }) ];
    };
}
