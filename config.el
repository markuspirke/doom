(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("6:00" . doom-feather-light)
                           ("10:00" . doom-bluloco-dark)))
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

(doom/set-frame-opacity 95) ;; Sets the transparency to 90% at start of doom.
;; (setq doom/set-frame-opacity 90)

(use-package nerd-icons
  :ensure t)

(good-scroll-mode t)

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

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
    ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

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

(defun mp/snakemake-open-hook ()
  "Hook to be run when Snakefile is opened"
   (when (string-match-p "Snakefile" (buffer-file-name))
    (snakemake-mode)))

(add-hook 'find-file-hook 'mp/snakemake-open-hook)

(use-package jinx
  :ensure t
  :hook ((LaTeX-mode . jinx-mode)
         (latex-mode . jinx-mode)
         (org-mode . jinx-mode)
         (text-mode . jinx-mode))
  ;; :config
  ;; (setq jinx-languages '("en_US" "de"))
  )
;; this turns of the flyspell-mode when an org document is opened
(remove-hook 'org-mode-hook #'flyspell-mode)
;; Shortcut for correct word
(map! :leader
      (:prefix ("e" . "edit")
      :desc "Correct word"
      "w" #'jinx-correct-word))

;; (require 'mu4e)
(use-package mu4e
        :ensure nil
        :config
        (setq mu4e-change-filenames-when-moving t)
        (setq mu4e-update-interval (* 10 60))
        (setq mu4e-get-mail-command "mbsync -a")
        (setq mu4e-maildir (expand-file-name "~/Mail"))
        (setq mu4e-headers-show-threads nil)
        (setq mu4e-headers-include-related nil)

        ;; Make sure plain text mails flow correctly for recipients
        (setq mu4e-compose-format-flowed t)

        ;; Configure the function to use for sending mail
        (setq message-send-mail-function 'smtpmail-send-it)
        )

(setq mu4e-contexts
      (list
       ;; Work account
       (make-mu4e-context
        :name "FAU"
        :enter-func (lambda () (mu4e-message "Switched to Work context"))
        :match-func (lambda (msg)
                (when msg
                        (string-prefix-p "/FAU" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "markus.pirke@fau.de")
                (user-full-name    . "Markus Pirke")
                (smtpmail-smtp-server  . "smtp-auth.fau.de")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-sent-folder  . "/FAU/Sent")
                (mu4e-drafts-folder . "/FAU/Drafts")
                (mu4e-trash-folder  . "/FAU/Trash")
                (mu4e-refile-folder . "/FAU/Archive")))

       (make-mu4e-context
        :name "iCloud"
        :enter-func (lambda () (mu4e-message "Switched to Personal context"))
        :match-func (lambda (msg)
                (when msg
                        (string-prefix-p "/iCloud" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "markus.pirke@icloud.com")
                (user-full-name    . "Markus Pirke")
                (mu4e-sent-folder  . "/iCloud/Sent Messages")
                (mu4e-drafts-folder . "/iCloud/Drafts")
                (mu4e-trash-folder  . "/iCloud/Deleted Messages")
                (mu4e-refile-folder . "/iCloud/Archive")))))

;; Highlights all matches of the selection in the buffer.
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
;; incrementally add the next unmatched match.
(define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;; Match selected region.
(define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;; Insert marker at point
(define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

;; Same as M-d but in reverse.
(define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
(define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)

;; OPTIONAL: If you prefer to grab symbols rather than words, use
;; `evil-multiedit-match-symbol-and-next` (or prev).

;; Restore the last group of multiedit regions.
(define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

;; RET will toggle the region under the cursor
(define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; ...and in visual mode, RET will disable all fields outside the selected region
(define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; For moving between edit regions
(define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
(define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

(setq +latex-viewers '(pdf-tools))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(after! latex
  (add-hook 'LaTeX-mode-hook #'lsp!))

(defun my/copy-latex-math-region ()
  "Copy the content between the nearest pair of $...$ surrounding point."
  (interactive)
  (save-excursion
    (let (beg end)
      ;; Search backward for opening $
      (unless (search-backward "$" nil t)
        (error "No opening $ found"))
      ;; Make sure it's not $$
      (while (looking-back "\\$" 1)
        (backward-char)
        (unless (search-backward "$" nil t)
          (error "No opening $ found")))
      (forward-char)
      (setq beg (point))
      ;; Search forward for closing $
      (unless (search-forward "$" nil t)
        (error "No closing $ found"))
      (setq end (1- (point)))
      (kill-ring-save beg end)
      (message "Copied LaTeX math content: %s" (buffer-substring-no-properties beg end)))))

(use-package! evil-textobj-anyblock
  :after evil
  :config
  ;; Define a $...$ text object using evil-textobj-anyblock
  (define-key evil-inner-text-objects-map "$"
    (evil-textobj-anyblock--make-textobj ?$))
  (define-key evil-outer-text-objects-map "$"
    (evil-textobj-anyblock--make-textobj ?$ t)))

(use-package! cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex))

;; (defun display-image-in-emacs (filename)
;;   "Open an image file inside Emacs."
;;   (let ((buffer (get-buffer-create "*Julia Plot*")))
;;     (with-current-buffer buffer
;;       (erase-buffer)
;;       (insert-image (create-image filename)))
;;     (display-buffer buffer)))
(defun display-image-in-emacs (filename)
  "Open an image file inside Emacs in a specific window."
  (let ((buffer (get-buffer-create "*Julia Plot*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert-image (create-image filename)))
    (display-buffer buffer)))

(defun setup-julia-layout ()
  "Set up a Julia workflow layout:
  - Left: Julia source file (big window)
  - Right (top): Plot preview
  - Right (bottom): vterm"
  (interactive)
  (when (string-match "\\.jl\\'" (buffer-file-name))  ;; Ensure it's a Julia file
    (let* ((main-window (frame-root-window))          ;; Get the main window
           (right-window (split-window main-window nil 'right))  ;; Split right side
           (plot-window (split-window right-window nil 'above))) ;; Split top-right

      ;; Keep left window large
      (select-window main-window)
      ;; (enlarge-window-horizontally (- (/ (window-width) 2) 50))

      ;; Open plot preview in top-right window
      (select-window plot-window)
      (switch-to-buffer (get-buffer-create "*Julia Plot*"))

      ;; Open vterm in the bottom-right window
      (select-window right-window)
      (vterm/here)  ;; Just open vterm, user starts Julia manually
      (select-window main-window))))
