#!/usr/bin/env bash
set -euo pipefail

cabal clean

cabal build exe:mu

BIN="$(cabal list-bin exe:mu)"
install -m 0755 -D "$BIN" "$HOME/.local/bin/mu"

echo "Installed:"
"$HOME/.local/bin/mu" --version 2>/dev/null || true
echo "From build artifact:"
echo "$BIN"
