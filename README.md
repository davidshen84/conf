# README

My *nix configuration repository. 😊

## Emacs Setup

All the required packages will be automatically pulled by the `use-package` package

### fix zsh with tramp

    [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

### Set font face per buffer with mode-line

    Local Variables:
    eval: (face-remap-add-relative 'default :family "AR PL KaitiM GB" :foundry "ARPH")
    End:

## stow

Set the current folder to the repository root.

```sh
stow -v -S -d .  -t ~/.config/ .config
```
