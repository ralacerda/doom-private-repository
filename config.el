;;; .config/doom/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(defun racl/maybe-insert-assign (arg)
  "Insert the first element of `ess-assign-list' only:
- If the point is not inside a string or comment
- The char before point is a space

If the character before point is the first element of
`ess-assign-list', replace it with the last character typed."

  (interactive "p")
  (let* ((assign ess-assign-list)
         (event (event-basic-type last-input-event))
         (char (ignore-errors (format "%c" event))))
    (cond ((and char
                (or
                 (not (equal 32 (char-before)))
                 (ess-inside-string-or-comment-p)))
           (insert char))
          ((re-search-backward assign (- (point) (length
                                                  assign)) t)
           (if (and char (numberp event))
               (replace-match char t t)
             (replace-match "")))
          (t (insert assign)))))

(defun racl/maybe-insert-pipe (arg)
  "Insert the first element of `ess-assign-list' only:
- If the point is not inside a string or comment
- The char before point is a space

If the character before point is the first element of
`ess-assign-list', replace it with the last character typed."

  (interactive "p")
  (let* ((pipe ess-assign-pipe)
         (event (event-basic-type last-input-event))
         (char (ignore-errors (format "%c" event))))
    (cond ((and char
                (or
                 (not (equal 32 (char-before)))
                 (ess-inside-string-or-comment-p)))
           (insert char))
          ((re-search-backward pipe (- (point) (length
                                                  pipe)) t)
           (if (and char (numberp event))
               (replace-match char t t)
             (replace-match "")))
          (t (insert pipe)))))


(map! :map ess-mode-map
      "_" #'racl/maybe-insert-assign
      "%" #'racl/maybe-insert-pipe)

(setq ess-assign-list "<- ")
(setq ess-assign-pipe "%>% ")

(defun racl/scroll-down ()
  (interactive)
  (scroll-up-command 5)
  )

(defun racl/scroll-up ()
  (interactive)
  (scroll-up-command -5)
  )

(defun racl/open-dropbox-file ()
  (interactive)
  (counsel-find-file "~/Dropbox")
    )

(setq scroll-preserve-screen-position 1)

(setq scroll-margin 5)

(setq doom-theme 'doom-dracula)

(global-set-key (kbd "C-v") #'racl/scroll-down)
(global-set-key (kbd "M-v") #'racl/scroll-up)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-:") 'comment-or-uncomment-region)

(global-set-key (kbd "C-S-q") 'avy-goto-char-timer)
(global-set-key (kbd "C-q") 'avy-goto-word-1)


(map! "C-s"   #'swiper

      "C-h"   #'backward-delete-char-untabify
      "C-M-h" #'backward-kill-word

      "C-="   #'er/expand-region
      "C--"   #'er/contract-region

      "C-'"   #'imenu

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
        "C-s"        (λ! (company-search-abort) (company-filter-candidates)))

      ;; "C-?"   #'+popup/toggle ; For some reason this doesn't work
      "C-x p"   #'+popup/toggle

      (:after smartparens
        :map smartparens-mode-map

        "C-M-a"     #'sp-beginning-of-sexp
        "C-M-e"     #'sp-end-of-sexp
        "C-M-d"     #'sp-splice-sexp
        "C-M-k"     #'sp-kill-sexp
        "C-M-t"     #'sp-transpose-sexp)

        "C-x k"     #'doom/kill-this-buffer-in-all-windows

        )

(setq doom-leader-alt-key (kbd "C-c"))

(map! :leader
      (:prefix-map ("e" . "Emacs")
        :desc "Restart emacs" "r" #'doom/restart
        :desc "Find config files" "d" #'doom/find-file-in-private-config)

      (:prefix-map ("o" . "open")
        :desc "Open recent files" "r" #'recentf-open-files
        :desc "Save bookmark" "n" #'bookmark-set
        :desc "Load bookmark" "o" #'counsel-bookmark
        :desc "List bookmarks" "l" #'bookmark-bmenu-list
        :desc "Open Dropbox file" "d" #'racl/open-dropbox-file))

(select-frame-set-input-focus (selected-frame))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package! doom-modeline
  :config
  (setq doom-modeline-major-mode-icon t))

(use-package! rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(setq +doom-dashboard-banner-dir (concat (dir!) "/banners/"))
(setq +doom-dashboard-banner-file "logo.png")

  (after! org
  (setq org-startup-indented nil)
  (setq org-adapt-indentation nil)
  (setq org-startup-folded "showall"))

(map! :map org-mode-map
      "C-M-n"       #'org-metadown
      "C-M-p"       #'org-metaup
      "C-M-S-n"     #'org-shiftmetadown
      "C-M-S-p"     #'org-shiftmetaup

      "C-M-f"       #'org-metaright
      "C-M-b"       #'org-metaleft
      "C-M-S-f"     #'org-shiftmetaright
      "C-M-S-b"     #'org-shiftmetaleft

      "C-c TAB"     #'org-insert-heading)

(setq doc-view-resolution 300)

;; Set $DICPATH to "$HOME/Library/Spelling" for hunspell.
(setenv
  "DICPATH"
  (concat (getenv "HOME") "/Library/Spelling"))
;; Tell ispell-mode to use hunspell.
(setq
  ispell-program-name
  "hunspell")
