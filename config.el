;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "Hack" :size 16))

(setq doom-theme 'doom-opera)

(custom-theme-set-faces! 'doom-opera

(setq display-line-numbers-type nil)

;;
(setq-default cursor-type 'bar)
(setq-default line-spacing 1)

;; Scrolling
(setq scroll-preserve-screen-position 1)

;; Backup Configuration
(setq kept-old-version 2)
(setq kept-newest-version 10)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq version-control t)

;; Auto-rever since we are not locking files
(setq create-lockfiles nil)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(setq set-fill-column 90)

(setq +doom-dashboard-banner-dir (concat (dir!) "/banners/"))
(setq +doom-dashboard-banner-file "logo.png")

(setq! show-paren-delay 1)
;; Packages
(use-package! which-key
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 5)
  (setq which-key-idle-secondary-delay 0.05))

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq doom-leader-alt-key "C-c")
(setq doom-localleader-alt-key "C-c l")

(load!"keys.el")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq company-idle-delay nil)

(setq doom-modeline-buffer-encoding nil)

(use-package! flyspell
  :config
  (remove-hook! '(org-mode-hook
                  markdown-mode-hook
                  TeX-mode-hook
                  rst-mode-hook
                  mu4e-compose-mode-hook
                  message-mode-hook
                  git-commit-mode-hook)
    #'flyspell-mode))

(use-package! hl-todo
  :hook (markdown-mode . hl-todo-mode))

(custom-set-faces! `(git-gutter-fr:modified :foreground ,(doom-color 'yellow)))
