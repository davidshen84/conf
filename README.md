# README

My *nix configuration repository. 😊

## Emacs Setup

All the required packages will be automatically pulled by the `use-package` package

### fix zsh with tramp

    [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

### Create ctags file
At your project root directory, execute this command to create tags file.

    `ctags -e --verbose -R --fields="+afikKlmnsSzt"`


### Windows context menu integration

**Require WSL**

If you want to create a `Open with GNU Emacs` shortcut for all types
of files. Update the path in the `OpenWithEmacs.reg` file, make sure
**not** to touch the arguments. Double click on the file to apply the
change.

If you want to remove the integration, double click on the
`OpenWithEmacs - Delete.reg` file.

### Swap `CapsLock` and `Ctrl_L` key on Windows

- Execute `caps-ctrl-swap.reg` to enable
- Execute `caps-ctrl-swap - Delete.reg` to disable

**Reboot is required!!!**

### Set font face per buffer with mode-line

    Local Variables:
    eval: (face-remap-add-relative 'default :family "AR PL KaitiM GB" :foundry "ARPH")
    End:

## Cygwin Environment Setup
*not updated*

To get a nice `zsh` environment on Windows, create a shortcut and set the `Target` as:

    x:\path\to\cygwin\bin\mintty.exe -i /Cygwin-Terminal.ico -e /bin/zsh -li
    
- `-l` for login so you have the user environment
- `-i` for interactive session

And set the `Start in` property to the path of your Cygwin user home directory.

To launch `Emacs` without a command window, create a shortcut and set the `Target` as:

    x:\path\to\cygwin\bin\run.exe zsh -lic emacs

And set the `Start in` property as well.
