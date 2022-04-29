;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Visual options
(setq doom-font (font-spec :family "Hack" :size 16))
(setq doom-theme 'doom-opera)
(setq display-line-numbers-type nil)
(setq set-fill-column 90)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; A nice banner in the dashboard
(setq +doom-dashboard-banner-dir (concat (dir!) "/banners/"))
(setq +doom-dashboard-banner-file "logo.png")

;; Backup Configuration
(setq kept-old-version 2)
(setq kept-newest-version 10)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq version-control t)

;; Auto-revert since we are not locking files. This means if some other
;; application modifies the file, the buffer will update to match it
(setq create-lockfiles nil)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)


(setq! show-paren-delay 1)

;; Extra package configuration
(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 5)
(setq which-key-idle-secondary-delay 0.05)

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(setq doom-modeline-buffer-encoding nil)

(setq-hook! 'ess-mode-hook
    ess-indent-with-fancy-comments nil)

(use-package! hl-todo
  :hook (markdown-mode . hl-todo-mode))

(custom-set-faces! `(git-gutter-fr:modified :foreground ,(doom-color 'yellow)))

(font-lock-add-keywords 'markdown-mode
    '(("@[[:word:]]*" . font-lock-string-face)))

(setq next-screen-context-lines 30)

(setq flymake-start-on-flymake-mode nil)

(map!

 "M-o"                        #'other-window

 "C-s"                        #'consult-line
 "C-x C-r"                    #'consult-recent-file

 "C-x l"                      #'consult-buffer

 "M-u"                        #'upcase-dwim
 "M-l"                        #'downcase-dwim
 "M-c"                        #'capitalize-dwim

 "C-h"                        #'backward-delete-char-untabify
 "M-h"                        #'backward-kill-word

 "M-p"                        #'mark-paragraph

 "C-x k"                      #'doom/kill-this-buffer-in-all-windows

 "C-q"                        #'avy-goto-word-1
 "M-j"                        #'avy-goto-char-timer

 "C-x p"                      #'+popup/close

 "C-."                        #'company-complete

 "C-z"                        #'magit-status

  )
