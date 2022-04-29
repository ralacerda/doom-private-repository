;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom!

  :completion
  company
  (vertico +icons)

  :emacs
  dired
  undo
  vc
  tree

  :checkers
  syntax

  :ui
  doom-dashboard
  doom
  modeline
  (popup +defaults)
  ophints
  treemacs
  ophints

  :editor
  snippets
  format

  :lang
  markdown
  org
  python
  emacs-lisp
  sh

  :term
  vterm

  :tools
  magit
  lookup

  :config
  (default +bindings +smartparens))
