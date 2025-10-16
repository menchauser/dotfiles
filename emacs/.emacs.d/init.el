;; Optimize startup
(setq gc-cons-threshold most-positive-fixnum)

;; Set path for customization settings file
(setq custom-file
			(expand-file-name "customize.el" user-emacs-directory))
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load-file custom-file)

;; Hide tool bar
(tool-bar-mode -1)

;; Yes or No
(fset 'yes-or-no-p 'y-or-n-p)


;; Use packages
(require 'package)
;; Load packages from Melpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install required package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ; avoid writing :ensure t everywhere

(use-package undo-fu)

;; Evil Mode
(use-package evil
	:init
	(setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;; Color themes
(use-package solarized-theme :defer t)
(use-package zenburn-theme :defer t)
(use-package basic-theme :defer t)
(use-package eink-theme :defer t)

;; Markdown Support
(use-package markdown-mode
	:defer t 
  :mode (("README\\.md\\'" . gfm-mode)
				 ("\\.md\\'" . markdown-mode)
				 ("\\.markdown\\'" . markdown-mode))
	:hook (markdown-mode . turn-on-auto-fill)
  :custom
  (markdown-command "multimarkdown"))

;; Use exec-path from shell PATH
(use-package exec-path-from-shell
	:if (memq window-system '(mac ns x))
	:config 
  (exec-path-from-shell-initialize))

;; Helm mode
(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files))
  :config
	(helm-mode 1)
  ;; Do not use helm for gptel command - it's not working
  (add-to-list 'helm-completing-read-handlers-alist
               '(gptel . nil)))

;; Find File In Project
(use-package find-file-in-project)

;; Perspectives
(use-package perspective
	:custom
	(persp-mode-prefix-key (kbd "C-c M-p"))
  :config
	(persp-mode t))

;; Slime
(use-package slime
	:defer t
	:bind
	(("C-c h" . slime-hyperspec-lookup))
	:custom
	(inferior-lisp-program "sbcl")
  :config
  (load "~/quicklisp/clhs-use-local.el" t))

(use-package magit :defer t)

(use-package projectile
	:defer t 
	:bind-keymap
	("C-c p" . projectile-command-map)
	:config
	(projectile-mode +1))

(use-package erlang
	:defer t
	:custom
	(erlang-root-dir "/usr/local/opt/erlang"))

(use-package password-store)

(use-package persistent-scratch
	:config
	(persistent-scratch-setup-default))

(use-package elpy
	:defer t 
	:hook
	(python-mode . elpy-enable))

(use-package fzf
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
  :init
  (yas-global-mode 1))

(use-package company
	:init
	(global-company-mode)
	:custom
	(company-idle-delay 0)
	(company-echo-delay 0)
	(company-minimum-prefix-length 1)
	:bind
	([(control return)] . company-complete))

(use-package lsp-mode
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
	:defer t
	:custom
	(rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
	:config
	(setq rustic-format-on-save t)
	(add-hook 'rustic-mode-hook
						(lambda () (electric-pair-mode 1))))

(use-package sweeprolog :defer t)

(use-package gptel
	:bind
	("C-c g" . gptel-send)
  :init
  ;; Load custom prompts to inject them in config
  (let ((custom-prompts-file 
         (expand-file-name "gptel-custom-prompts.el" user-emacs-directory)))
    (when (file-exists-p custom-prompts-file)
      (load-file custom-prompts-file)))
	:config
  ;; Merge custom prompts with built-in ones
  (when (boundp 'my-gptel-custom-directives)
    (setq gptel-directives
          (append gptel-directives my-gptel-custom-directives)))
	;; define provider backends
	;; Claude
	(setq gptel-anthropic
				(gptel-make-anthropic "Claude"
					:stream t
					:key #'gptel-api-key-from-auth-source))
	;; Use Claude by default
	(setq gptel-backend gptel-anthropic
				gptel-model 'claude-sonnet-4-5-20250929)
	;; Common settings
	(setq
	 ;; Use org syntax
	 gptel-default-mode 'org-mode
	 ;; Include referred files in context
	 gptel-track-media t
   ;; Allow debugging of requests
   gptel-expert-commands t)
	;; Move cursor after response
	(add-hook 'gptel-post-response-functions 'gptel-end-of-response)
	;; Use llm-tool collection
	;; (add-to-list 'load-path "/Users/mkaranashev/p/oss/llm-tool-collection")
	;;(require 'llm-tool-collection)
	;; (mapcar (apply-partially #'apply #'gptel-make-tool)
	;; (llm-tool-collection-get-all))
  ;; MCP
  (require 'gptel-integrations)
	;; Custom Tools
	(gptel-make-tool
	 :name "find_files"
	 :function (lambda (pattern &optional directory)
							 "Find files matching PATTERN in DIRECTORY (default: current dir)"
							 (let* ((dir (or directory default-directory))
											(cmd (format "find %s -type f -name '%s' 2>/dev/null"
																	 (shell-quote-argument dir)
																	 pattern))
											(results (shell-command-to-string cmd)))
								 (if (string-empty-p results)
										 (format "No files found matching '%s' in %s" pattern dir)
									 results)))
	 :description "Find files by pattern in a directory. Use shell wildcards like *.el or test*.txt"
	 :args (list '(:name "pattern"
											 :type string
											 :description "File pattern to search for (e.g., '*.el', 'test*.txt')")
							 '(:name "directory"
											 :type string
											 :optional t
											 :description "Directory to search in (optional, defaults to current directory)"))
	 :category "filesystem"))

;; Org-mode configuration. It is built-in so no need to ensure it
(use-package org
	:ensure nil ; Built-in
	:hook ((org-mode . visual-line-mode)   ; soft-wrapping
				 (org-mode . display-fill-column-indicator-mode)
				 (org-mode . auto-fill-mode)
				 (org-mode . (lambda () (company-mode -1))))
	:custom
	(indent-tabs-mode nil) ; do not use tabs for indentation
	(fill-column 100)
	(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
	(browse-url-browser-function 'browse-url-default-browser))

(use-package evil-org
	:after org
	:hook (org-mode . (lambda () (evil-org-mode)))
	:config
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys))

(use-package ultra-scroll
	:ensure t
	:init
	(setq scroll-conservatively 3
				scroll-margin 0)
	:config
	(ultra-scroll-mode 1))

;; for claude code
(use-package inheritenv :defer t
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

;; for claude code: terminal backend
(use-package eat :defer t)
;; for claude code: vterm terminal backend:
(use-package vterm :defer t)

;; claude code
(use-package claude-code :defer t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (claude-code-mode)
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))
  

;;;; USE-PACKAGE ENDS HERE ;;;;

;; gptel tools

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

;; Keybindings set using use-package for simplicity
(use-package emacs
	:ensure nil ; built in
	:bind
	(("<C-tab>" . other-window)
	 ("C-c q" . persp-switch)
	 ("C-x C-m" . execute-extended-command)
	 ("C-x C-2" . toggle-frame-maximized)
	 ("C-x m" . nil))) ; Unbind compose mail not to interfere with my org-mode stuff

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

;; Load private config if it exists
(let ((private-file (expand-file-name "private.init.el" user-emacs-directory)))
	(when (file-exists-p private-file)
		(load-file private-file)))

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
