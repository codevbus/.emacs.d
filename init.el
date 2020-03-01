;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; my naive init.el, shamelessly cribbed from others

;;;; -*- lexical-binding: t -*-

;;; Code:

;;;; Straight Config
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

;;;; Use-package Setup
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

;;; Misc config
(transient-mark-mode 1)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Visual config
(global-display-line-numbers-mode t)
(set-frame-font "Source Code Pro For Powerline-14" nil t)
(with-eval-after-load "treemacs"
  (defvar treemacs-custom-tf-icon (all-the-icons-icon-for-file
				   "file.tf"))
  (treemacs-define-custom-icon treemacs-custom-tf-icon "tf" "tfvars" "tfstate"))

(use-package all-the-icons)
(use-package emojify)

;;;; Ivy configuration
(use-package ivy)

;;; Counsel
(use-package counsel
  :demand t
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-c i" . counsel-imenu)
   ("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)
   ("C-x k" . kill-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-x l" . counsel-locate)
   ("C-c j" . counsel-git)
   ("C-c s" . counsel-rg)
   ("M-y" . counsel-yank-pop)
   :map help-map
   ("f" . counsel-describe-function)
   ("v" . counsel-describe-variable)
   ("l" . counsel-info-lookup-symbol)
   :map ivy-minibuffer-map
   ("C-o" . ivy-occur)
   ("<return>" . ivy-alt-done)
   ("M-<return>" . ivy-immediate-done)
   :map read-expression-map
   ("C-r" . counsel-minibuffer-history))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist
   '((t . ivy--regex-plus)))
  :config
  (ivy-mode +1))

;;;; Org mode configuration
(use-package org)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-export-headline-levels 4)
;; Org mode workflow state
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; Kbd shortcuts
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c [") 'org-agenda-file-to-front)
(global-set-key (kbd "C-c ]") 'org-remove-file)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c w") 'toggle-window-split)

;; org files
(setq org-local "~/Documents/org/")
(setq org-shared "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
(setq org-agenda-files (list org-local
			     (concat org-shared "inbox.org")))
(setq org-refile-targets '((nil :maxlevel . 9)
      (org-agenda-files :maxlevel . 9)))

(setq org-capture-templates
      `(("w" "Work Task" entry (file+headline ,(concat org-local "inbox.org") "Tasks")
	 "* TODO %?")
	("n" "Work Note" entry (file+headline ,(concat org-local "inbox.org") "Notes")
	 "* %?")
	("t" "Personal Todo" entry (file+headline ,(concat org-shared "inbox.org") "To-do")
	 "* TODO %?")
	("c" "Code Task" entry (file+headline ,(concat org-local "inbox.org") "Tasks")
	 "* TODO %?\n %i\n  %a")
	("l" "link" entry (file+headline, (concat org-shared "inbox.org") "Resources")
	 "** TODO %(org-cliplink-capture) \n CREATED: %t\n" :immediate-finish t)))

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
      (org-roam-directory (concat org-shared "2b/org"))
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
  (org-journal-dir (concat org-shared "2b/org"))
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-cliplink)

(use-package org-ql)

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

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package base16-theme
  :config
  (load-theme 'base16-materia t))

(use-package evil
  :config
  (evil-mode 1))

(use-package magit)

(use-package ox-hugo
  :after ox)

(use-package winum)
(winum-mode)

;;;; Treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package direnv)

(use-package flycheck
  :config
  (global-flycheck-mode +1)

  (setq-default flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled))

  ;; Temporary workaround: Direnv needs to load PATH before flycheck looks
  ;; for linters
  (setq flycheck-executable-find
        (lambda (cmd)
          (direnv-update-environment default-directory)
          (executable-find cmd))))

(use-package flycheck-hydra
  :straight nil
  :no-require t
  :after flycheck hydra
  :config
  (defhydra jethro/hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("n"  flycheck-next-error                                       "Next")
    ("p"  flycheck-previous-error                                   "Previous")
    ("<" flycheck-first-error                                      "First")
    (">"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))

  (bind-key "C-c h f" #'jethro/hydra-flycheck/body))

(use-package flycheck-pos-tip
  :after flycheck
  :hook
  (flycheck-mode . flycheck-pos-tip-mode))

(flycheck-add-mode 'proselint 'org-mode)

(use-package yasnippet
  :blackout ((yas-global-mode . t)
             (yas-minor-mode . t))
  :config
  (yas-global-mode +1)
  :custom
  (add-to-list 'load-path (expand-file-name "snippets" user-emacs-directory))
  (require 'yasnippet-snippets)
  (yas-snippet-dirs (list (expand-file-name "snippets/snippets" user-emacs-directory))))

(use-package company
  :blackout company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
         ("C-/" . company-yasnippet)
         :map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.5)
  (company-require-match nil)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  :init
  (global-company-mode +1))

(use-package company-quickhelp
  :after company
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin))
  :hook
  (company-mode . company-quickhelp-mode))

(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-after-open-hook . lsp-enable-imenu)
  (go-mode . lsp)
  (python-mode . lsp)
  (terraform-mode . lsp)
  (yaml-mode . lsp)
  :custom
  (lsp-message-project-root-warning t))

(use-package lsp-ui
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :after (company lsp)
  :company lsp-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package f)

;;;; Languages

;;; Hashicorp Configuration Language
(use-package hcl-mode)

;;; Terraform
(use-package terraform-mode)

;;; Python
(use-package python-mode)

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

;;; Groovy
(use-package groovy-mode)

;;;; Functions
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
