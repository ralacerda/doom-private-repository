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

(setq scroll-preserve-screen-position 1)

(setq scroll-margin 5)

(setq doom-theme 'doom-dracula)

(global-set-key (kbd "C-v") #'racl/scroll-down)
(global-set-key (kbd "M-v") #'racl/scroll-up)

(map! "C-s"   #'swiper

      "C-h"   #'backward-delete-char-untabify
      "C-M-h" #'backward-kill-word

      "C-="   #'er/expand-region
      "C--"   #'er/contract-region

      "C-'"   #'imenu

      ;; "C-?"   #'+popup/toggle ; For some reason this doesn't work
      "C-x p"   #'+popup/toggle
      (:after smartparens
        :map smartparens-mode-map

        "C-M-a"     #'sp-beginning-of-sexp
        "C-M-e"     #'sp-end-of-sexp
        "C-M-f"     #'sp-forward-sexp
        "C-M-b"     #'sp-backward-sexp
        "C-M-d"     #'sp-splice-sexp
        "C-M-k"     #'sp-kill-sexp
        "C-M-t"     #'sp-transpose-sexp)

        "C-x k"     #'doom/kill-this-buffer-in-all-windows

        )

(setq doom-leader-alt-key (kbd "C-c"))

(map! :leader
      (:prefix-map ("e" . "emacs")
        :desc "Restart emacs" "r" #'doom/restart
        :desc "Find config files" "d" #'doom/find-file-in-private-config)
  )


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
