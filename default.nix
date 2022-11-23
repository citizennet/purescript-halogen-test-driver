{ self, ... }: {
  perSystem = { config, ... }: {
    packages.halogen-test-driver = config.purs-nix-build ./.;
  };
}
