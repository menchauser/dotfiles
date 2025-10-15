;; Set path for customization settings file
(setq custom-file "~/.emacs.d/customize.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load-file custom-file)

;; create server
;; (require 'server)
;; (unless (server-running-p)
  ;; (server-start))

;; Hide tool bar
(tool-bar-mode -1)

;; Yes or No
(defalias 'yes-or-no-p 'y-or-n-p)

;; Custom functions which are used package configuration
;; Open journal file at start
(require 'calendar)

(defun my-open-journal-file ()
	(interactive)
	(let* ((calendar-date-display-form
					'((format "%s%.2d" year (string-to-number month))))
				 (journal-path (concat "~/p/journal/"
															 (calendar-date-string
																(calendar-current-date))
															 ".org")))
		(find-file journal-path)
		(end-of-buffer)))

;; Custom Jira link function
(defun my-org-insert-jira-link ()
	(interactive)
	(let* ((jira-link (read-string "Jira link: "))
				 (ticket-code (car (last (split-string jira-link "/")))))
		(org-insert-link nil jira-link ticket-code)))

;; Custom date header function
(defun my-org-insert-daily-title ()
	(interactive)
	(let* ((calendar-date-display-form
					'((format "%s-%.2d-%.2d %s"
										year
										(string-to-number month)
										(string-to-number day)
										(substring dayname 0 3)))))
		(insert 
		 (concat "* "
						 (calendar-date-string (calendar-current-date))))))


;; Keys for custom functions and other options
;; TODO: delete
(defun my-org-mode-hook ()
  (setq fill-column 80)
  ;; (fci-mode t)
	(display-fill-column-indicator-mode t)
  (auto-fill-mode t)
	(local-set-key (kbd "C-x C-y") 'my-org-insert-jira-link)
	(local-set-key (kbd "C-c pd") 'my-org-insert-daily-title)
	(setq browse-url-browser-function 'browse-url-default-browser)
	(company-mode -1))


;; Use packages
(require 'package)
;; Load packages from Melpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install required package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package undo-fu
	:ensure t)

;; Evil Mode
(use-package evil
  :ensure t
	:init
	(setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;; Color themes
(use-package solarized-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package basic-theme :ensure t :defer t)
(use-package eink-theme :ensure t :defer t)

;; Markdown Support
(use-package markdown-mode
  :ensure t
	:defer t 
  :mode (("README\\.md\\'" . gfm-mode)
				 ("\\.md\\'" . markdown-mode)
				 ("\\.markdown\\'" . markdown-mode))
	:hook (markdown-mode . turn-on-auto-fill)
  :custom
  (markdown-command "multimarkdown"))

;; Use exec-path from shell PATH
(use-package exec-path-from-shell
  :ensure t
	:if (memq window-system '(mac ns x))
	:config 
  (exec-path-from-shell-initialize))

;; Helm mode
(use-package helm
  :ensure t
  :config
	(helm-mode 1))

;; Find File In Project
(use-package find-file-in-project
	:ensure t)

;; Perspectives
(use-package perspective
  :ensure t
	:custom
	(persp-mode-prefix-key (kbd "C-c M-p"))
  :config
	(persp-mode t))

  ;; Ido
(use-package ido
  :ensure t
  :config
  (ido-mode t))


;; Slime
(use-package slime
  :ensure t
	:defer t
	:bind
	(("C-c h" . slime-hyperspec-lookup))
	:custom
	(inferior-lisp-program "sbcl")
  :config
  (load "~/quicklisp/clhs-use-local.el" t))
  
(use-package magit
	:ensure t
	:defer t)

(use-package projectile
	:ensure t
	:defer t 
	:bind-keymap
	("C-c p" . projectile-command-map)
	:config
	(projectile-mode +1))

(use-package erlang
	:ensure t
	:defer t
	:custom
	(erlang-root-dir "/usr/local/opt/erlang"))

(use-package password-store
	:ensure t)

(use-package persistent-scratch
	:ensure t
	:config
	(persistent-scratch-setup-default))

(use-package elpy
	:ensure t
	:defer t 
	:init
	(elpy-enable))

(use-package fzf
	:ensure t
	:bind
	("C-c r" . fzf)
	:config
	(setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
				fzf/executable "fzf"
				fzf/git-grep-args "-i --line-number %s"
				fzf/grep-command "grep -nrH"
				fzf/position-bottom t
				fzf/window-height 15))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package company
	:ensure t
	:init
	(global-company-mode)
	:custom
	(company-idle-delay 0)
	(company-echo-delay 0)
	(company-minimum-prefix-length 1)
	:bind
	([(control return)] . company-complete))

(use-package lsp-mode
	:ensure t
	:defer t
	:bind
	("M-RET" . lsp-execute-code-action))

;; (use-package lsp-ui
;; 	:ensure t
;; 	:after lsp-mode
;; 	:custom
;; 	(lsp-ui-sideline-show-code-actions t)
;;   (lsp-ui-doc-position 'at-point))

;; (use-package lsp-treemacs
;; 	:ensure t
;;   :after lsp-mode
;;   :config
;;   (lsp-treemacs-sync-mode 1))

(use-package rustic
	:ensure t
	:defer t
	:custom
	(rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
	:config
	(setq rustic-format-on-save t)
	(add-hook 'rustic-mode-hook
						(lambda () (electric-pair-mode 1))))

(use-package sweeprolog
	:ensure t
	:defer t)

(use-package gptel
  :ensure t
	:config
	(setq gptel-default-mode 'org-mode)
	(setq gptel-model 'claude-sonnet-4-5-20250929
				gptel-backend (gptel-make-anthropic "Claude"
												:stream t
												:key #'gptel-api-key-from-auth-source))
	(setq gptel-track-media t))

;; Org-mode configuration. It is built-in so no need to ensure it
(use-package org
	:ensure nil ; Built-in
	:hook ((org-mode . visual-line-mode)   ; soft-wrapping
				 (org-mode . display-fill-column-indicator-mode)
				 (org-mode . auto-fill-mode)
				 (org-mode . (lambda () (company-mode -1))))
	:bind
	(:map org-mode-map
				("C-x C-y" . my-org-mode-insert-jira-link)
				("C-c pd" . my-org-insert-daily-title))
	:custom
	(fill-column 120)
	(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
	(browse-url-browser-function 'browse-url-default-browser))

(use-package evil-org
	:ensure t
	:after org
	:hook (org-mode . (lambda () (evil-org-mode)))
	:config
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys))

	
;;;; USE-PACKAGE ENDS HERE ;;;;

;; Factor
(setq fuel-factor-root-dir "~/factor")

;; (setq custom-tab-width 2)
(defun disable-tabs () (setq indent-tabs-mode nil))

;; Lisp-mode: configure to not use tabs
(add-hook 'lisp-mode-hook
					(lambda ()
						(disable-tabs)
						(setq fill-column 80)
						(display-fill-column-indicator-mode t)))
						;; (fci-mode t)))

;; Keybindings
(global-set-key (kbd "C-<tab>") 'other-window)
;; Unset "compose mail" not to interfere with my org-mode stuff
(keymap-global-unset "C-x m")
;; https://emacs.stackexchange.com/a/358
;; https://nullprogram.com/blog/2013/02/06/
(define-minor-mode my-keys-mode
  "My keybindings"
  :lighter " keys"
  :init-value t
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-tab>") 'other-window)
    (define-key map (kbd "C-c q") 'persp-switch)
		(define-key map (kbd "C-x C-m") 'execute-extended-command)
		(define-key map (kbd "C-x C-2") 'toggle-frame-maximized)
    map))
(define-globalized-minor-mode global-my-keys-mode my-keys-mode my-keys-mode)
(provide 'my-keys-mode)


;; Mode settings
;; org-mode
;; Soft wrapping for org-mode

;; Configure encryption
;;(require 'org-crypt)
(require 'epa-file)
(epa-file-enable)

;; ispell
;; (setenv
;;  "DICPATH"
;;  "~/local/dictionaries")
;; (setq ispell-program-name "~/local/bin/hunspell")
;; (setq ispell-dictionary "en_US")

;; WebKit Browser
;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)

;; smooth scrolling
(pixel-scroll-precision-mode 1)

;; final step
(my-open-journal-file)
