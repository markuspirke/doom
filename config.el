(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("6:00" . doom-feather-light)
                           ("10:00" . doom-palenight)))
  (circadian-setup))

;; Linenumber
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative) ;; This sets relative line numbers.

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                org-agenda-mode-hook
                pdf-view-mode-hook ;; otherwise conflicts
                shell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

;;
;; I prefer visual lines
(setq display-line-numbers-type 'visual
      line-move-visual t)
(use-package-hook! evil
  :pre-init
  (setq evil-respect-visual-line-mode t) ;; sane j and k behavior
  t)

;; I also like evil mode visual movement
(map! :map evil-normal-state-map
      :desc "Move to next visual line"
      "j" 'evil-next-visual-line
      :desc "Move to previous visual line"
      "k" 'evil-previous-visual-line)

(remove-hook 'org-mode-hook #'vi-tilde-fringe-mode)

;; (use-package treesit-auto
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all))

;; No titlebar
(add-to-list 'default-frame-alist '(undecorated-round . t))

(doom/set-frame-opacity 90) ;; Sets the transparency to 90% at start of doom.
;; (setq doom/set-frame-opacity 90)

(use-package nerd-icons
  :ensure t)

(define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)

(use-package org
        :config
        (setq org-agenda-start-with-log-mode t)
        (setq org-log-done 'time)
        (setq org-log-into-drawer t)
        (setq org-directory "~/Org/") ;; last / was necessary
        (setq org-agenda-files '("todos.org"))
        (setq org-agenda-hide-tags-regexp ".*")
        (setq org-agenda-span 7)
        (setq org-agenda-start-day "+0d")
        (setq org-agenda-time-grid '((daily) () "" ""))
        (setq org-todo-keywords
        '((sequence "TODO(t)" "READ(r)" "|" "DONE(d)")))
)

(custom-set-faces!
  `(outline-1 :height 1.3 :foreground ,(nth 1 (nth 14 doom-themes--colors)))
  `(outline-2 :height 1.25 :foreground ,(nth 1 (nth 15 doom-themes--colors)))
  `(outline-3 :height 1.2 :foreground ,(nth 1 (nth 19 doom-themes--colors)))
  `(outline-4 :height 1.1 :foreground ,(nth 1 (nth 23 doom-themes--colors)))
  `(outline-5 :height 1.1 :foreground ,(nth 1 (nth 24 doom-themes--colors)))
  `(outline-6 :height 1.1 :foreground ,(nth 1 (nth 16 doom-themes--colors)))
  `(outline-7 :height 1.05 :foreground ,(nth 1 (nth 18 doom-themes--colors)))
  `(outline-8 :height 1.05 :foreground ,(nth 1 (nth 11 doom-themes--colors)))
  )

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 org-modern-fold-stars '(("◉" . "◉") ("○" . "○") ("●" . "●") ("○" . "○"))

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "◀── now ─────────────────────────────────────────────────")

;; Ellipsis styling
(setq org-ellipsis " ▾") ;; symbol if header is closed
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
(with-eval-after-load 'org (global-org-modern-mode))

(defun mp/org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

(add-hook 'org-agenda-mode-hook 'mp/org-agenda-open-hook)

;; Custom styles for dates in agenda
(custom-set-faces!
  '(org-agenda-date :inherit outline-1 :height 1.01)
  '(org-agenda-date-today :inherit outline-2 :height 1.01)
  '(org-agenda-date-weekend :inherit outline-1 :height 1.01)
  '(org-agenda-date-weekend-today :inherit outline-2 :height 1.01)
  '(org-super-agenda-header :inherit custom-button :weight bold :height 1.01)
  `(link :foreground unspecified :underline nil :background ,(nth 1 (nth 7 doom-themes--colors)))
  '(org-link :foreground unspecified))

;; this determines what is shown in the agenda
(setq org-agenda-prefix-format '(
  (agenda . " %?-2i %t ")
  (todo . " %i %-12:c")
  (tags . " %i %-12:c")
  (search . " %i %-12:c")))

;; different emojis for different categories of todos
(setq org-agenda-category-icon-alist
      '(("work" (nerd-icons-faicon "" :height 0.8 :v-adjust 0) nil nil :ascent center)
        ("teaching" (nerd-icons-faicon "" :height 0.8 :v-adjust 0) nil nil :ascent center)
        ("home" (nerd-icons-mdicon "󰏚󰠧" :height 0.8 :v-adjust 0) nil nil :ascent center)
        ("privat" (nerd-icons-mdicon "󰏚" :height 0.8 :v-adjust 0) nil nil :ascent center)
        ))

(require 'org-super-agenda)
(setq org-super-agenda-groups
      '(
        (:name " Today "
               :time-grid t
               :date today
               :scheduled today
               :order 1
               :face 'warning)
        (:name "! Overdue "
               :scheduled past
               :date today
               :order 1
               ;; :not (:log closed)
               ;; :discard (:todo "DONE")
               :face 'error)
        (:name "Teaching "
               :and(:category "teaching")
               :order 3)
        (:name "Haus "
               :and(:category "home")
               :order 3)
        (:name "Privat "
               :and(:category "private")
               :order 3)
        (:name "Arbeit"
               :and(:category "work")
               :order 3)
        ))

(org-super-agenda-mode t)

(map! :desc "Next Line"
      :map org-super-agenda-header-map
      "j" 'org-agenda-next-line)

(map! :desc "Next Line"
      :map org-super-agenda-header-map
      "k" 'org-agenda-previous-line)

(defun mp/tangle-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-babel-tangle)))

(add-hook 'after-save-hook 'mp/tangle-on-save-org-mode-file)

(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

  ;; (use-package org-bullets
  ;;   :hook (org-mode . org-bullets-mode)
  ;;   :custom
  ;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-roam-directory "~/Zettelkasten")

(set-file-template! 'org-mode :ignore t) ;; works

(use-package! org-roam-ui
  :after org-roam
  )

(setq org-journal-dir "~/Tagebuch")

  (defun mp/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

  (use-package visual-fill-column
    :hook (org-mode . mp/org-mode-visual-fill))

;; Enable LSP mode for Julia
(setq lsp-julia-package-dir nil)
(setq lsp-julia-default-environment "~/.julia/environments/v1.11")

(use-package jinx
  :ensure t
  :hook ((LaTeX-mode . jinx-mode)
         (latex-mode . jinx-mode)
         (org-mode . jinx-mode)
         (text-mode . jinx-mode)))
;; this turns of the flyspell-mode when an org document is opened
(remove-hook 'org-mode-hook #'flyspell-mode)

;; (require 'mu4e)
(use-package mu4e
        :ensure nil
        :config
        (setq mu4e-change-filenames-when-moving t)
        (setq mu4e-update-interval (* 10 60))
        (setq mu4e-get-mail-command "mbsync -a")
        (setq mu4e-maildir-list '("$HOME/Mail"))
        )
