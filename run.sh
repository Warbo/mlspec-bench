#! /usr/bin/env nix-shell
#! nix-shell -i bash -p cabal2nix
nix-shell -E "$(cabal2nix --shell ./.)" --run "cabal run"
