(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("6:00" . doom-one-light)
                           ("15:00" . doom-palenight)))
  (circadian-setup))

;; Linenumber
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative) ;; This sets relative line numbers.

;; No titlebar
(add-to-list 'default-frame-alist '(undecorated-round . t))

(doom/set-frame-opacity 90) ;; Sets the transparency to 90% at start of doom.
;; (setq doom/set-frame-opacity 90)

(use-package nerd-icons
  :ensure t)

(use-package org
        :config
        (setq org-ellipsis " ▾") ;; symbol if header is closed
        (setq org-agenda-start-with-log-mode t)
        (setq org-log-done 'time)
        (setq org-log-into-drawer t)
        (setq org-directory "~/Org/") ;; last / was necessary
        (setq org-agenda-files '("todos.org"))
        (setq org-agenda-hide-tags-regexp ".*")
)

(with-eval-after-load 'org (global-org-modern-mode))

(defun mp/org-agenda-open-hook ()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

(add-hook 'org-agenda-mode-hook 'mp/org-agenda-open-hook)

(setq org-agenda-prefix-format '(
  (agenda . " %?-2i %t ")
  (todo . " %i %-12:c")
  (tags . " %i %-12:c")
  (search . " %i %-12:c")))

(setq org-agenda-category-icon-alist
      '(("work" (nerd-icons-faicon "" :height 0.8 :v-adjust 0) nil nil :ascent center)
        ("teaching" (nerd-icons-faicon "" :height 0.8 :v-adjust 0) nil nil :ascent center)
        ("home" (nerd-icons-mdicon "󰏚󰠧" :height 0.8 :v-adjust 0) nil nil :ascent center)
        ("privat" (nerd-icons-mdicon "󰏚" :height 0.8 :v-adjust 0) nil nil :ascent center)
        ))

  ;; Automatically tangle our Emacs.org config file when we save it
  ;; (defun doom/org-babel-tangle-config ()
  ;;   (when (string-equal (file-name-directory (buffer-file-name))
  ;;                       (expand-file-name user-emacs-directory))
  ;;     ;; Dynamic scoping to the rescue
  ;;     (let ((org-confirm-babel-evaluate nil))
  ;;       (org-babel-tangle))))

  ;; (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'doom/org-babel-tangle-config)))

(defun mp/tangle-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-babel-tangle)))

(add-hook 'after-save-hook 'mp/tangle-on-save-org-mode-file)

  ;; (use-package org-bullets
  ;;   :hook (org-mode . org-bullets-mode)
  ;;   :custom
  ;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-roam-directory "~/Zettelkasten")

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
