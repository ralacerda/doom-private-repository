;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "Hack" :size 16))

(setq doom-theme 'doom-opera)

(setq display-line-numbers-type nil)

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



(setq set-fill-column 70)

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

(global-set-key (kbd "C-v") (kbd "<mouse-5>"))
(global-set-key (kbd "M-v") (kbd "<mouse-4>"))

(map!

 "M-o"       #'other-window

 "C-s"       #'swiper
 "C-x C-r"   #'counsel-recentf

 "C-x C-l"   #'er-switch-to-previous-buffer
 "C-x l"     #'ivy-switch-buffer

 "M-u"       #'upcase-dwim
 "M-l"       #'downcase-dwim
 "M-c"       #'capitalize-dwim

 "C-h"       #'backward-delete-char-untabify
 "M-h"       #'backward-kill-word

 "M-p"       #'mark-paragraph
 
 "C-="       #'er/expand-region
 "C--"       #'er/contract-region

 "C-'"       #'imenu

 "C-x k"     #'doom/kill-this-buffer-in-all-windows

 "C-a"       #'doom/backward-to-bol-or-indent
 "C-e"       #'doom/forward-to-last-non-comment-or-eol

 "C-x C-o"   ctl-x-4-map

 "<C-mouse-4>" #'text-scale-increase
 "<C-mouse-5>" #'text-scale-decrease

 "C-S-q"     #'avy-goto-char-timer
 "C-q"       #'avy-goto-word-1


 (:after company
         :map company-active-map
         "C-n"        #'company-select-next
         "C-p"        #'company-select-previous
         "C-s"        #'company-search-candidates
         "M-s"        #'company-filter-candidates
         "<C-tab>"    #'company-complete-common-or-cycle
         [tab]        #'company-complete-common-or-cycle
         [backtab]    #'company-select-previous
         "C-RET"      #'counsel-company

         :map company-search-map
         "C-n"        #'company-search-repeat-forward
         "C-p"        #'company-search-repeat-backward
         "C-s"        (λ! (company-search-abort) (company-filter-candidates))))
