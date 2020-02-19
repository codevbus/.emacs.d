;;;; -*- lexical-binding: t -*-
;;; Straight setup
(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

;;; Use-package Setup
(straight-use-package 'use-package)
(use-package blackout
  :straight (blackout :host github :repo "raxod502/blackout")
  :demand t)

(use-package el-patch
  :straight (:host github
                   :repo "raxod502/el-patch"
                   :branch "develop"))
(eval-when-compile
  (require 'el-patch))

(use-package use-package-company
  :straight (use-package-company :host github :repo "akirak/use-package-company"))

;; Some combination of GNU TLS and Emacs fail to retrieve archive
;; contents over https.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341

(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;;;;Visual config
(global-display-line-numbers-mode t)
(set-frame-font "Source Code Pro For Powerline-14" nil t)

;;;; Helm configuration
;; Enable Helm
(use-package helm)

;; Set default M-x to Helm
(global-set-key (kbd "M-x") 'helm-M-x)

;;;;Org mode configuration
;; Enable Org mode
(use-package org)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
;; Org mode workflow state
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
;; Kbd shortcuts
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c [") 'org-agenda-file-to-front)
(global-set-key (kbd "C-c ]") 'org-remove-file)
;; org agenda files
(setq org-agenda-files '("~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org"))

(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list
   '("◉" "○")))

;; Org roam
(use-package org-roam
      :after org
      :hook 
      ((org-mode . org-roam-mode)
       (after-init . org-roam--build-cache-async) ;; optional!
       )
      :straight (:host github :repo "jethrokuan/org-roam" :branch "develop")
      :custom
      (org-roam-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/2b/org")
      :bind
      ("C-c n l" . org-roam)
      ("C-c n t" . org-roam-today)
      ("C-c n f" . org-roam-find-file)
      ("C-c n i" . org-roam-insert)
      ("C-c n g" . org-roam-show-graph))

;; Org journal
(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/2b/org")
  (org-journal-date-format "%A, %d %B %Y"))

;; Deft
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/2b/org"))

;;;;Misc config
;; Set the exec path from the shell path
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-materia t))

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package magit
   :ensure t)

(setq custom-file "~/.emacs.d/package-selected-packages.el")
(load custom-file)
