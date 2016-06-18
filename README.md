# README #

my personal gist :)

## Cygwin Environment Setup

To get a nice `zsh` environment on Windows, create a shortcut and set
the `Target` as:

    x:\path\to\cygwin\bin\mintty.exe -i /Cygwin-Terminal.ico -e /bin/zsh -li
    
- -l for login so you have the user environment
- -i for interactive session

And set the `Start in` property to the path of your Cygwin user home directory.

To launch `Emacs` without a command window, create a shortcut and set the `Target` as:

    x:\path\to\cygwin\bin\run.exe zsh -lic emacs

And set the `Start in` property as well.

## Emacs Setup

### Cygwin system dependencies

  - emacs-clang-format

### ELPA package dependencies

  - highlight-indentation
  - js2-mode
  - json-mode
  - org *aka org-mode*
  - auto-complete
  - auto-complete-exuberant-ctags

        In your project root directory, do follow command to make tags file.

            etags --verbose -R --fields="+afikKlmnsSzt"

  - markdown-mode
  - editorconfig

### Windows context menu integration

If you want to create a `Open with GNU Emacs` shortcut for all types
of files. Update the path in the `OpenWithEmacs.reg` file, make sure
**not** to touch the arguments. Double click on the file to apply the
change.

If you want to remove the integration, double click on the
`OpenWithEmacs - Delete.reg` file.

