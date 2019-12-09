(after! smartparens
  ;; Smartparens is broken in `cc-mode' as of Emacs 27. See
  ;; <https://github.com/Fuco1/smartparens/issues/963>.
  (unless EMACS27+
    (pushnew! sp--special-self-insert-commands 'c-electric-paren 'c-electric-brace))

  ;; Smartparens' navigation feature is neat, but does not justify how
  ;; expensive it is. It's also less useful for evil users. This may need to
  ;; be reactivated for non-evil users though. Needs more testing!
  (add-hook! 'after-change-major-mode-hook
    (defun doom-disable-smartparens-navigate-skip-match-h ()
      (setq sp-navigate-skip-match nil
            sp-navigate-consider-sgml-tags nil)))

  ;; Autopair quotes more conservatively; if I'm next to a word/before another
  ;; quote, I likely don't want to open a new pair.
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  ;; Expand {|} => { | }
  ;; Expand {|} => {
  ;;   |
  ;; }
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; I likely don't want a new pair if adjacent to a word or opening brace
             :unless '(sp-point-before-word-p sp-point-before-same-p)))

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Major-mode specific fixes
  (sp-local-pair '(ruby-mode enh-ruby-mode) "{" "}"
                 :pre-handlers '(:rem sp-ruby-pre-handler)
                 :post-handlers '(:rem sp-ruby-post-handler))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))

  ;; Reasonable default pairs for HTML-style comments
  (sp-local-pair (append sp--html-modes '(markdown-mode gfm-mode))
                 "<!--" "-->"
                 :unless '(sp-point-before-word-p sp-point-before-same-p)
                 :actions '(insert) :post-handlers '(("| " "SPC")))

  ;; Disable electric keys in C modes because it interferes with smartparens
  ;; and custom bindings. We'll do it ourselves (mostly).
  (after! cc-mode
    (c-toggle-electric-state -1)
    (c-toggle-auto-newline -1)
    (setq c-electric-flag nil)
    (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")" "\177"))
      (define-key c-mode-base-map key nil))

    ;; Smartparens and cc-mode both try to autoclose angle-brackets
    ;; intelligently. The result isn't very intelligent (causes redundant
    ;; characters), so just do it ourselves.
    (define-key! c++-mode-map "<" nil ">" nil)

    (defun +default-cc-sp-point-is-template-p (id action context)
      "Return t if point is in the right place for C++ angle-brackets."
      (and (sp-in-code-p id action context)
           (cond ((eq action 'insert)
                  (sp-point-after-word-p id action context))
                 ((eq action 'autoskip)
                  (/= (char-before) 32)))))

    (defun +default-cc-sp-point-after-include-p (id action context)
      "Return t if point is in an #include."
      (and (sp-in-code-p id action context)
           (save-excursion
             (goto-char (line-beginning-position))
             (looking-at-p "[ 	]*#include[^<]+"))))

    ;; ...and leave it to smartparens
    (sp-local-pair '(c++-mode objc-mode)
                   "<" ">"
                   :when '(+default-cc-sp-point-is-template-p
                           +default-cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC")))

    (sp-local-pair '(c-mode c++-mode objc-mode java-mode)
                   "/*!" "*/"
                   :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC"))))

  ;; Expand C-style doc comment blocks. Must be done manually because some of
  ;; these languages use specialized (and deferred) parsers, whose state we
  ;; can't access while smartparens is doing its thing.
  (defun +default-expand-asterix-doc-comment-block (&rest _ignored)
    (let ((indent (current-indentation)))
      (newline-and-indent)
      (save-excursion
        (newline)
        (insert (make-string indent 32) " */")
        (delete-char 2))))
  (sp-local-pair
   '(js2-mode typescript-mode rjsx-mode rust-mode c-mode c++-mode objc-mode
              csharp-mode java-mode php-mode css-mode scss-mode less-css-mode
              stylus-mode scala-mode)
   "/*" "*/"
   :actions '(insert)
   :post-handlers '(("| " "SPC")
                    ("|\n[i]*/[d-2]" "RET")
                    (+default-expand-asterix-doc-comment-block "*")))

  (after! smartparens-ml
    (sp-with-modes '(tuareg-mode fsharp-mode)
      (sp-local-pair "(*" "*)" :actions nil)
      (sp-local-pair "(*" "*"
                     :actions '(insert)
                     :post-handlers '(("| " "SPC") ("|\n[i]*)[d-2]" "RET")))))

  (after! smartparens-markdown
    (sp-with-modes '(markdown-mode gfm-mode)
      (sp-local-pair "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

      ;; The original rules for smartparens had an odd quirk: inserting two
      ;; asterixex would replace nearby quotes with asterixes. These two rules
      ;; set out to fix this.
      (sp-local-pair "**" nil :actions :rem)
      (sp-local-pair "*" "*"
                     :actions '(insert skip)
                     :unless '(:rem sp-point-at-bol-p)
                     ;; * then SPC will delete the second asterix and assume
                     ;; you wanted a bullet point. * followed by another *
                     ;; will produce an extra, assuming you wanted **|**.
                     :post-handlers '(("[d1]" "SPC") ("|*" "*"))))

    ;; This keybind allows * to skip over **.
    (map! :map markdown-mode-map
          :ig "*" (λ! (if (looking-at-p "\\*\\* *$")
                          (forward-char 2)
                        (call-interactively 'self-insert-command)))))

  ;; Highjacks backspace to:
  ;;  a) balance spaces inside brackets/parentheses ( | ) -> (|)
  ;;  b) delete up to nearest column multiple of `tab-width' at a time
  ;;  c) close empty multiline brace blocks in one step:
  ;;     {
  ;;     |
  ;;     }
  ;;     becomes {|}
  ;;  d) refresh smartparens' :post-handlers, so SPC and RET expansions work
  ;;     even after a backspace.
  ;;  e) properly delete smartparen pairs when they are encountered, without
  ;;     the need for strict mode.
  ;;  f) do none of this when inside a string
  (advice-add #'delete-backward-char :override #'+default*delete-backward-char))

;; Makes `newline-and-indent' continue comments (and more reliably)
(advice-add #'newline-and-indent :override #'+default*newline-indent-and-continue-comments)
