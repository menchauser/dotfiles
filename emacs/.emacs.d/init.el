;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
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

;; default font
(set-face-attribute 'default nil :font "Berkeley Mono-14")

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
;; (use-package helm
;;   :bind
;;   (("M-x" . helm-M-x)
;;    ("C-x C-f" . helm-find-files))
;;   :config
;; 	(helm-mode 1)
;;   ;; Do not use helm for gptel command - it's not working
;;   (add-to-list 'helm-completing-read-handlers-alist
;;                '(gptel . nil)))
;; Let's use Vertico instead
(use-package vertico
  :custom
  ((completion-ignore-case t)
   (read-buffer-completion-ignore-case t))
  :config
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode))

;; Route 'completion-at-point through Vertico 
(use-package consult
  :config
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))


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

;; For better performance with projectile 
(use-package which-key
  :config
  (which-key-mode +1))

;; Projectile
(use-package projectile
  :defer t 
  :init
  (setq projectile-project-search-path '("~/p . 1"))
  :bind-keymap
  ("s-p" . projectile-command-map)
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

;; (use-package elpy
;; 	:defer t 
;; 	:hook
;; 	(python-mode . elpy-enable))

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
	  :key #'gptel-api-key-from-auth-source
          :request-params '(:max_tokens 16000
                                        :thinking (:type "enabled"
                                                         :budget_tokens 10000))))
  ;; Use Claude by default
  (setq gptel-backend gptel-anthropic
	gptel-model 'claude-sonnet-4-6)
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
   :name "read_url"
   :function (lambda (url)
               (with-current-buffer (url-retrieve-synchronously url)
                 (goto-char (point-min))
                 (forward-paragraph)
                 (let ((dom (libxml-parse-html-region (point) (point-max))))
                   (run-at-time 0 nil #'kill-buffer (current-buffer))
                   (with-temp-buffer
                     (shr-insert-document dom)
                     (buffer-substring-no-properties (point-min) (point-max))))))
   :description "Fetch and read the contents of a URL"
   :args (list '(:name "url" :type string :description "The URL to read"))
   :category "web"))

;; Org-mode configuration. It is built-in so no need to ensure it
(use-package org
  :ensure nil ; Built-in
  :hook ((org-mode . visual-line-mode)   ; soft-wrapping
	 (org-mode . display-fill-column-indicator-mode)
	 (org-mode . auto-fill-mode)
	 (org-mode . (lambda () (company-mode -1))))
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
                                        ; Unbind C-' as it is used by avy
  (define-key org-mode-map (kbd "C-'") nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))
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

;; JSON indentation
(use-package json-mode
  :custom
  (js-indent-level 2))

;; Haskell
(use-package eglot
  :hook
  ((haskell-mode . eglot-ensure)
   (python-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c w" . eglot-rename)
              ("C-c a" . eglot-code-actions))
  :config
  (setq-default eglot-workspace-configuration
                '(:haskell (:plugin (:stan (:globalOn :json-false))    ; disable stan
                                    :formattingProvider "fourmolu")
                           :gopls (:staticcheck t :usePlaceholders t)))  ; use fourmolu instead of ormolu
  :custom
  (eglot-autoshutdown t) ; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil) ; allow edits without confirmation
  )

;; auto-fill only comments in Literate Haskell file, not the code
(defun my-literate-haskell-auto-fill ()
  "Auto-fill only non-code (bird-style) lines in literate Haskell."
  (unless (save-excursion
            (beginning-of-line)
            (looking-at "^>"))
    (do-auto-fill)))

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)
         (literate-haskell-mode . interactive-haskell-mode)
         (literate-haskell-mode . (lambda ()
                                    (auto-fill-mode 1)
                                    (setq-local auto-fill-function
                                                #'my-literate-haskell-auto-fill))))
  :bind
  ("C-x C-h" . haskell-hoogle))

(use-package avy
  :bind 
  ("C-'" . avy-goto-char-timer)
  )

;; I 
(use-package verb
  :defer t)

(use-package fuel
  :init
  (setq fuel-factor-root-dir "~/factor"))

;; Nice reading experience with word wrapping
(use-package olivetti
  :ensure t
  :hook (markdown-mode . olivetti-mode)
  :custom
  (olivetti-body-width 100))

(use-package agent-shell
  :ensure t
  :defer t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Go language support
(setq treesit-language-source-alist
      '((go "https://github.com/tree-sitter/tree-sitter-go" "v0.25.0")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        ;; ikatyang/tree-sitter-yaml is archived; this fork is the maintained
        ;; one (also what nvim-treesitter builds), and it matches yaml-ts-mode's
        ;; queries shipped with Emacs 30.
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

(use-package go-ts-mode
  :mode (("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . my/go-setup))
  :custom (go-ts-mode-indent-offset 4)
  :config
  (add-to-list 'exec-path
               (expand-file-name "~/go/bin"))
  (defun my/go-setup ()
    (setq-local tab-width 4)
    (add-hook 'before-save-hook #'my/go-before-save nil t))
  (defun my/go-before-save ()
    (when (eglot-managed-p)
      (ignore-errors (call-interactively #'eglot-code-action-organize-imports))
      (eglot-format-buffer))))


;; Ghostel terminal 
(use-package ghostel
  :ensure t
  :hook (after-init . ghostel-status-indicator-mode))

(use-package evil-ghostel
  :ensure t
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

;;;; USE-PACKAGE ENDS HERE ;;;;

;; Factor

;; (setq custom-tab-width 2)
(defun disable-tabs () (setq indent-tabs-mode nil))

;; Lisp-mode: configure to not use tabs
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (disable-tabs)
	    (setq fill-column 80)
	    (display-fill-column-indicator-mode t)))
;; (fci-mode t)))

;; Custom IPA enter via minibuffer
(defun my/insert-ipa ()
  "Enter IPA via minibuffer with the ipa-x-sampa input method.
If point is inside an existing [IPA] block, edit its contents;
otherwise insert a new [IPA] block."
  (interactive)
  (let ((bounds (my--ipa-block-bounds)))
    (if bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (new (my--read-ipa "IPA: " (buffer-substring-no-properties beg end))))
          (delete-region beg end)
          (goto-char beg)
          (insert new))
      (insert "[" (my--read-ipa "IPA: " nil) "]"))))

(defun my--read-ipa (prompt initial)
  "Read an IPA string with PROMPT and INITIAL input, using ipa-x-sampa."
  (minibuffer-with-setup-hook
      (lambda () (set-input-method "ipa-x-sampa"))
    (read-string prompt initial)))

(defun my--ipa-block-bounds ()
  "Return (BEG . END) of the contents of the [ ] pair around point on the
current line, or nil if point is not inside one."
  (let* ((pt (point))
         (open (save-excursion
                 ;; nearest [ before point, not closed before point
                 (when (and (search-backward "[" (line-beginning-position) t)
                            (not (search-forward "]" pt t)))
                   (point))))
         (close (and open
                     (save-excursion
                       (goto-char pt)
                       ;; nearest ] after point, with no [ in between
                       (when (and (search-forward "]" (line-end-position) t)
                                  (not (search-backward "[" pt t)))
                         (point))))))
    (when (and open close)
      (cons (1+ open) (1- close)))))

;; Keybindings set using use-package for simplicity
(use-package emacs
  :ensure nil ; built in
  :bind
  (("<C-tab>" . other-window)
   ("C-c q" . persp-switch)
   ("C-x C-m" . execute-extended-command)
   ("C-x C-2" . toggle-frame-maximized)
   ("C-c i p" . my/insert-ipa)
   ("C-x m" . nil))  ; Unbind compose mail not to interfere with my org-mode stuff
) 

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

;; disable audible bell, use display-bell
(setq visible-bell t)

;; save minibuffer history
(setq history-length 25)
(savehist-mode 1)

;; dont pop UI dialog when prompting
(setq use-dialog-box nil)

;; Load private config if it exists
(let ((private-file (expand-file-name "private.init.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file)))

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

