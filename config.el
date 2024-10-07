(setq doom-theme 'doom-palenight)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative) ;; This sets relative line numbers.

(defvar mp/frame-transparency '(90 . 90))

  ;; Automatically tangle our Emacs.org config file when we save it
  (defun mp/org-babel-tangle-config ()
    (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name user-emacs-directory))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'mp/org-babel-tangle-config)))

  ;; (use-package org-bullets
  ;;   :hook (org-mode . org-bullets-mode)
  ;;   :custom
  ;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(setq org-roam-directory "~/Zettelkasten")
