;; -*- lexical-binding: t; -*-
;; (require 'benchmark-init)  
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

;; Set path for customization settings file
(setq custom-file
      (expand-file-name "customize.el" user-emacs-directory))
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load-file custom-file)


;; Short answers for yes-or-no prompts
(setq use-short-answers t)


;; Use packages
;; use-package ships with Emacs 30, so no need to install it separately.
(require 'package)
;; Load packages from Melpa
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (vertico-preselect 'prompt)
  (vertico-cycle t)
  (vertico-count 10)
  (vertico-resize t)
  :config
  (vertico-mode 1))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map 
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode 1))

;; Route 'completion-at-point through Vertico 
(use-package consult
  :config
  ;; `consult-completion-in-region' itself falls back to the default
  ;; completion UI when neither Vertico nor Icomplete is active.
  (setq completion-in-region-function #'consult-completion-in-region))

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
  :ensure nil
  :config
  (which-key-mode +1))

;; Projectile
(use-package projectile
  :defer t 
  :init
  (setq projectile-project-search-path '("~/p" . 1))
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
  :hook (rustic-mode . electric-pair-local-mode)
  :config
  (setq rustic-format-on-save t))

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
	gptel-model 'claude-opus-5-5)
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
(defun my/org-setup ()
  "Buffer-local settings for org-mode"
  (setq-local indent-tabs-mode nil) ;; do not use tabs for indentation
  (setq-local fill-column 100)
  ;; visual-line-mode copies this into a buffer-local `fringe-indicator-alist` when it turns on
  (setq-local visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (visual-line-mode 1)  ;; soft-wrapping
  (display-fill-column-indicator-mode 1)
  (auto-fill-mode 1)
  (company-mode -1))

(use-package org
  :ensure nil ; Built-in
  :hook (org-mode . my/org-setup)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
                                        ; Unbind C-' as it is used by avy
  (define-key org-mode-map (kbd "C-'") nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

;; PDF export (C-c C-e l p). Requires tectonic: brew install tectonic
(use-package ox-latex
  :ensure nil ; Built-in, part of org
  :after org
  :custom
  ;; XeTeX handles UTF-8 and system fonts natively (Croatian, Greek, IPA)
  (org-latex-compiler "xelatex")
  (org-latex-pdf-process '("tectonic -X compile --outdir=%o %f"))
  ;; Highlighted code blocks without needing pygments/minted
  (org-latex-src-block-backend 'listings)
  :bind (:map org-mode-map ("C-c P" . my/org-export-pdf-async))
  :config
  ;; Org (9.8) loads amssymb only for pdflatex but still emits $\square$ etc. for checkboxes
  (add-to-list 'org-latex-packages-alist '("" "amssymb" t ("xelatex" "lualatex")))
  ;; org emits lstlisting environments for the listings backend but doesn't load the package
  (add-to-list 'org-latex-packages-alist '("" "listings"))

  (defun my/org-export-pdf-async ()
    "Export the buffer to LaTeX, then compile it to PDF without blocking Emacs.
Runs the first command of `org-latex-pdf-process' in a compilation buffer,
so progress (e.g. tectonic package downloads) is visible live. Opens the
PDF when compilation succeeds."
    (interactive)
    (let* ((tex (expand-file-name (org-latex-export-to-latex)))
           (dir (file-name-directory tex))
           (pdf (concat (file-name-sans-extension tex) ".pdf"))
           (cmd (format-spec (car org-latex-pdf-process)
                             `((?f . ,(shell-quote-argument tex))
                               (?o . ,(shell-quote-argument (directory-file-name dir)))
                               (?b . ,(shell-quote-argument (file-name-base tex))))))
           (default-directory dir)
           (compilation-buffer-name-function (lambda (_) "*Org PDF*"))
           (buf (compile cmd)))
      (with-current-buffer buf
        (add-hook 'compilation-finish-functions
                  (lambda (_buf status)
                    (when (string-prefix-p "finished" status)
                      (org-open-file pdf)))
                  nil t)))))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package ultra-scroll
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
  :ensure nil
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
  (eglot-confirm-server-edits nil) ; allow edits without confirmation
  )

;; auto-fill only comments in Literate Haskell file, not the code
(defun my-literate-haskell-auto-fill ()
  "Auto-fill only non-code (bird-style) lines in literate Haskell."
  (unless (save-excursion
            (beginning-of-line)
            (looking-at "^>"))
    (do-auto-fill)))

(defun my/haskell-literate-setup ()
  (auto-fill-mode 1)
  (setq-local auto-fill-function #'my-literate-haskell-auto-fill))

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-literate-mode . my/haskell-literate-setup))
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
  :hook (markdown-mode . olivetti-mode)
  :custom
  (olivetti-body-width 100))

(use-package agent-shell
  :defer t)

(use-package expand-region
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
  :hook (after-init . ghostel-status-indicator-mode))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

;; Ancient Greek (LSJ) and Latin dictionaries with morphological parsing.
;; Reads the data bundled with Diogenes.app (https://d.iogen.es).
;; Installed by hand rather than with :vc, because its Package-Requires header
;; is malformed and package.el refuses it ("Version must be a string"):
;;   git clone https://github.com/nitardus/diogenes.el ~/.emacs.d/lisp/diogenes.el
(use-package diogenes
  :ensure nil
  :load-path "lisp/diogenes.el"
  :init
  ;; Both must be set before the package loads: it validates the paths on load.
  (setq diogenes-path "/Applications/Diogenes.app/Contents"
        ;; Diogenes 4.7 ships the LSJ under this name, not the package default.
        diogenes-preferred-lsj-file "grc.lsj.xml")
  ;; No package.el autoloads for a hand-installed package, so list M-x entry points.
  :commands (diogenes diogenes-lookup-greek diogenes-parse-greek)
  :bind
  ;; Parses inflected forms to their lemma first, then shows the LSJ entry.
  ("C-c d" . diogenes-parse-and-lookup-greek)
  :config
  ;; Workaround for an upstream bug. The morphology files are sorted bytewise
  ;; on the full Beta Code form, accents included (Diogenes' own Perseus.pm
  ;; compares them that way), but `diogenes--parse-word' binary-searches them
  ;; ignoring accents and breathings. The search then lands in the wrong place
  ;; and misses forms that are there: οἴκει never offered οἰκέω.
  ;; Compare exactly first; keep the accent-blind search as a fallback for
  ;; input typed without diacritics.
  (defvar my/diogenes-exact-compare nil
    "When non-nil, `diogenes--ascii-sort-function' compares whole keys.")
  (define-advice diogenes--ascii-sort-function (:around (orig a b) my/exact)
    (if my/diogenes-exact-compare
        (let ((a (downcase a)) (b (downcase b)))
          (cond ((string-lessp b a) 'a)
                ((string-lessp a b) 'b)))
      (funcall orig a b)))
  (define-advice diogenes--parse-word (:around (orig word lang) my/exact)
    (let ((exact (let ((my/diogenes-exact-compare t))
                   (funcall orig word lang))))
      (if (nth 3 exact) exact (funcall orig word lang)))))

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

;; Extra Ancient Greek dictionaries.  Diogenes can only show the LSJ, so other
;; dictionaries go through sdcv, which reads every StarDict dictionary found
;; in ~/.stardict/dic (built with ~/.stardict/make-greek-stardict.py; their
;; display order is set in ~/.config/sdcv_ordering).  Diogenes still does the
;; parsing, because those dictionaries only know headwords, not inflected forms.
(defun my/greek-dict-lookup (word)
  "Show the entries for Greek WORD in every StarDict dictionary sdcv knows.
WORD may be inflected: Diogenes reduces it to its lemma first."
  (interactive (list (read-string "Greek word (extra dictionaries): "
                                  (thing-at-point 'word t))))
  (let* ((lemma (my--greek-lemma word))
         ;; Ask for the typed form as well: it can be a headword in its own
         ;; right, and one dictionary lists inflected verb forms.
         (words (delete-dups (list lemma word)))
         (entries (or (my--greek-sdcv (mapcan #'my--greek-spellings words))
                      ;; Nothing under that accentuation, for instance because
                      ;; the word was typed without any: match the bare letters.
                      (my--greek-sdcv (mapcar #'my--greek-bare words)))))
    (unless entries
      (user-error "No entry for %s in the dictionaries under ~/.stardict/dic" lemma))
    ;; Show the buffer before filling it: HTML is wrapped to its window's width.
    (pop-to-buffer (get-buffer-create "*greek-dict*"))
    (progn
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (entry entries)
          (let-alist entry
            (insert (propertize (format "%s  [%s]" .word .dict) 'face 'bold) "\n")
            (let ((start (point)))
              (insert (string-trim .definition) "\n")
              ;; StarDict definitions are plain text or HTML, depending on the dictionary.
              (when (string-match-p "<[[:alpha:]/]" .definition)
                (shr-render-region start (point-max))))
            (goto-char (point-max))
            (insert "\n")))
        (goto-char (point-min))))))

(defun my--greek-lemma (word)
  "Return the dictionary form of Greek WORD, as parsed by Diogenes."
  (require 'diogenes)
  (let (lemma)
    ;; Reuse Diogenes' parser and its lemma prompt, but catch the lemma it is
    ;; about to look up in the LSJ instead of letting it open that entry.
    (cl-letf (((symbol-function 'diogenes--lookup-dict)
               (lambda (found _lang) (setq lemma found))))
      (diogenes--parse-and-lookup (diogenes--greek-ensure-beta word) "greek"))
    ;; Diogenes answers in Beta Code and numbers homonyms (le/gw3).
    (replace-regexp-in-string
     "[0-9]+\\'" ""
     (diogenes--perseus-ensure-utf8 (or lemma word) "greek"))))

(defconst my--greek-tonos-to-oxia
  '((#x3AC . #x1F71) (#x3AD . #x1F73) (#x3AE . #x1F75) (#x3AF . #x1F77)
    (#x3CC . #x1F79) (#x3CD . #x1F7B) (#x3CE . #x1F7D) (#x390 . #x1FD3)
    (#x3B0 . #x1FE3) (#x386 . #x1FBB) (#x388 . #x1FC9) (#x389 . #x1FCB)
    (#x38A . #x1FDB) (#x38C . #x1FF9) (#x38E . #x1FEB) (#x38F . #x1FFB))
  "Acute-accented Greek vowels: the Greek block code point and its
identical-looking twin in the Greek Extended block.")

(defun my--greek-without-marks (word keep-p)
  "Return WORD minus the combining marks for which KEEP-P returns nil."
  (ucs-normalize-NFC-string
   (concat (seq-filter
            (lambda (char)
              (or (not (eq (get-char-code-property char 'general-category) 'Mn))
                  (funcall keep-p char)))
            (ucs-normalize-NFD-string word)))))

(defun my--greek-spellings (word)
  "Return the ways a dictionary may have encoded the headword WORD.
sdcv only matches headwords byte for byte, and dictionaries disagree on
which of Unicode's two acute accents to use: normalized text has the one
from the Greek block, Diogenes emits the one from Greek Extended."
  (let* (;; Dictionaries index headwords without macrons and breves.
         (tonos (my--greek-without-marks
                 word (lambda (char) (not (memq char '(#x304 #x306))))))
         (oxia (apply #'string
                      (mapcar (lambda (char)
                                (alist-get char my--greek-tonos-to-oxia char))
                              tonos))))
    (delete-dups (list tonos oxia))))

(defun my--greek-bare (word)
  "Return WORD in lower case without accents, breathings or length marks.
The dictionaries built by make-greek-stardict.py index that form too."
  (downcase (my--greek-without-marks word #'ignore)))

(defun my--greek-sdcv (words)
  "Return sdcv's exact matches for any of WORDS as a list of alists."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      ;; sdcv exits non-zero when a word has no match; that is not an error here.
      (apply #'call-process "sdcv" nil '(t nil) nil "-n" "-j" "-e" "-0" "-1" words))
    (goto-char (point-min))
    ;; One JSON array per queried word.
    (let (entries)
      (ignore-errors
        (while t
          (setq entries (append entries (json-parse-buffer :object-type 'alist
                                                           :array-type 'list)))))
      ;; Different spellings can find the same article.
      (seq-uniq entries
                (lambda (a b)
                  (and (equal (alist-get 'dict a) (alist-get 'dict b))
                       (equal (alist-get 'definition a) (alist-get 'definition b))))))))

;; Keybindings set using use-package for simplicity
(use-package emacs
  :ensure nil ; built in
  :bind
  (("<C-tab>" . other-window)
   ("C-c q" . persp-switch)
   ("C-x C-m" . execute-extended-command)
   ("C-x C-2" . toggle-frame-maximized)
   ("C-c i p" . my/insert-ipa)
   ("C-c D" . my/greek-dict-lookup)
   ("C-x m" . nil))  ; Unbind compose mail not to interfere with my org-mode stuff
  :custom
  (browse-url-browser-function 'browse-url-default-browser)) 

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

;; Setup custom font for greek input to avoid Helvetica
(defun my/setup-fontsets ()
  (set-fontset-font t 'greek (font-spec :family "Menlo"))
  (set-fontset-font t 'cyrillic (font-spec :family "Menlo")))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/setup-fontsets)
  (my/setup-fontsets))

;; Load private config if it exists
(let ((private-file (expand-file-name "private.init.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file)))


