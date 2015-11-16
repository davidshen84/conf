# README

my personal gist :)


## For Emacs

- Grab [highlight-indentation][1] and drop it at ~/.emacs.d/lisp
- Install the following from **Packages**
    - `js3-mode`
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


[1]: https://raw.githubusercontent.com/antonj/Highlight-Indentation-for-Emacs/master/highlight-indentation.el
