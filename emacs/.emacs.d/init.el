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
(use-package solarized-theme
  :ensure t)

(use-package zenburn-theme
	:ensure t)

(use-package basic-theme
	:ensure t)

(use-package eink-theme
	:ensure t)

;; Markdown Support
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  (add-hook 'markdown-mode-hook 'turn-on-auto-fill))

;; Use exec-path from shell PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Helm mode
(use-package helm
  :ensure t
  :config)

;; Find File In Project
(use-package find-file-in-project
	:ensure t
	:config)


;; Fill Column Indicator
;; we should use display-fill-column-indicator-mode
;; (use-package fill-column-indicator
;;   :ensure t)

;; Perspectives
(use-package perspective
  :ensure t
  ;;:bind
  ;;("C-x C-b" . persp-list-buffers)
  :config
	(progn
		(customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
		(persp-mode t)))

  ;; Ido
(use-package ido
    :ensure t
    :config
    (ido-mode t))


;; Slime
(use-package slime
  :ensure t
	:bind
	(("C-c h" . slime-hyperspec-lookup))
  :config
  (progn
    (setq inferior-lisp-program "sbcl")
    (load "~/quicklisp/clhs-use-local.el" t)))
  
(use-package magit
	:ensure t)

(use-package projectile
	:ensure t
	:config
	(progn
		(projectile-mode +1)
		(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(use-package erlang
	:ensure t
	:config
	(setq erlang-root-dir "/usr/local/opt/erlang"))

(use-package evil-org
	:ensure t
	:after org
	:hook (org-mode . (lambda () (evil-org-mode)))
	:config
	(require 'evil-org-agenda)
	(evil-org-agenda-set-keys))

(use-package password-store
	:ensure t)

(use-package persistent-scratch
	:ensure t
	:config
	(persistent-scratch-setup-default))

(use-package elpy
	:ensure t
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
	:custom
	(rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
	:config
	(setq rustic-format-on-save t)
	(add-hook 'rustic-mode-hook
						(lambda () (electric-pair-mode 1))))

(use-package sweeprolog
	:ensure t)
	
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

;; Configure encryption
;;(require 'org-crypt)
(require 'epa-file)
(epa-file-enable)

;; Open perspective with journal on start
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
(defun my-org-mode-hook ()
  (setq fill-column 80)
  ;; (fci-mode t)
	(display-fill-column-indicator-mode t)
  (auto-fill-mode t)
	(local-set-key (kbd "C-x C-y") 'my-org-insert-jira-link)
	(local-set-key (kbd "C-c pd") 'my-org-insert-daily-title)
	(setq browse-url-browser-function 'browse-url-default-browser)
	(company-mode -1))

(add-hook 'org-mode-hook 'my-org-mode-hook)

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
