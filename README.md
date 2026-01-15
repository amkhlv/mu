# mu

## CLI launcher

Reads list of commands from `~/.config/amkhlv/mu.yaml` at compile time, and turns them into subcommands of the executable.

Example of `~/.config/amkhlv/mu.yaml` :

```yaml
explore: |
  nu -c "cat ~/.config/amkhlv/mu.yaml | from yaml | explore"
edit: |
  gvim --nofork ~/.config/amkhlv/mu.yaml
  pushd ~/git/mu
  ./install.sh
  popd
completions: |
  mu --bash-completion-script mu > ~/.local/share/bash-completion/completions/mu
nix: 
  unfree: |
    NIXPKGS_ALLOW_UNFREE=1   nix-shell -p %1
  chrome: |
    NIXPKGS_ALLOW_UNFREE=1   nix-shell -p google-chrome
firefox:
  work: |
    firefox -P Work &
  fun: |
    firefox -P Fun &
```

Then, the command:

    mu firefox work

launches `firefox -P Work`.

The YAML file is read __at compile time__. Need to recompile using `install.sh` after each modification of `~/.config/amkhlv/mu.yaml`.

The main point is autocompletion, which can be set up by: 

    mu --bash-completion-script mu > ~/.local/share/bash-completion/completions/mu



