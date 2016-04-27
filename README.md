# README #

my personal gist :)


## For Emacs ##

### Install the following from **Packages** ###

  - `highlight-indentation`
  - `js2-mode`
  - `json-mode`
  - `org` *aka org-mode*
    - `auto-complete`
    - `auto-complete-exuberant-ctags`
      In your project root directory, do follow command to make tags file.

            etags --verbose -R --fields="+afikKlmnsSzt"
    - `markdown-mode`
    - `editorconfig` and `editorconfig-core`
      Use this configure to let editorconfig plugin loads the lisp
      version of editorconfig program
  
            (setq editorconfig-get-properties-function
                'editorconfig-core-get-properties-hash)
            (editorconfig-mode 1)


### Windows context menu integration ###

Update the path in `OpenWithEmacs.reg`, make sure do **not** touch the
arguments. Double click on the file to apply the change.

If you want to remove the integration, double click on the
`OpenWithEmacs - Delete.reg` file.
