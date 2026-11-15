;;; greek-study.el --- Ancient Greek study tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Dictionary lookup (Diogenes, StarDict) and PDF export of notes.
;; Every part depends on tools installed outside this repository, so each
;; one is set up only when its tools are found on this machine.  Loaded from
;; init.el; the checks run once, at startup.

;;; Code:

(defvar my/diogenes-app "/Applications/Diogenes.app/Contents"
  "Where Diogenes.app keeps its Perl code and dictionary data.")

(defvar my/diogenes-lisp
  (expand-file-name "lisp/diogenes.el" user-emacs-directory)
  "Checkout of https://github.com/nitardus/diogenes.el.")

(defvar my/stardict-dir (expand-file-name "~/.stardict/dic")
  "Where sdcv looks for StarDict dictionaries.")

(defun my/diogenes-available-p ()
  "Non-nil when both Diogenes.app and diogenes.el are installed."
  (and (file-directory-p my/diogenes-app)
       (file-exists-p (expand-file-name "diogenes.el" my/diogenes-lisp))))

(defun my/stardict-available-p ()
  "Non-nil when sdcv is installed and has at least one dictionary."
  (and (executable-find "sdcv")
       (file-directory-p my/stardict-dir)
       (directory-files-recursively my/stardict-dir "\\.ifo\\'")))

;;;; PDF export (C-c C-e l p, or C-c P to compile asynchronously)

;; Requires tectonic: brew install tectonic
(use-package ox-latex
  :ensure nil ; Built-in, part of org
  :if (executable-find "tectonic")
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

;;;; LSJ and Latin dictionaries with morphological parsing (C-c d)

;; Reads the data bundled with Diogenes.app (https://d.iogen.es).
;; Installed by hand rather than with :vc, because its Package-Requires header
;; is malformed and package.el refuses it ("Version must be a string"):
;;   git clone https://github.com/nitardus/diogenes.el ~/.emacs.d/lisp/diogenes.el
(use-package diogenes
  :ensure nil
  :if (my/diogenes-available-p)
  :load-path "lisp/diogenes.el"
  :init
  ;; Both must be set before the package loads: it validates the paths on load.
  (setq diogenes-path my/diogenes-app
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

;;;; Extra Ancient Greek dictionaries through sdcv (C-c D)

;; Diogenes can only show the LSJ, so other dictionaries go through sdcv,
;; which reads every StarDict dictionary found in ~/.stardict/dic (built with
;; ~/.stardict/make-greek-stardict.py; their display order is set in
;; ~/.config/sdcv_ordering).  When Diogenes is installed it still does the
;; parsing, because those dictionaries only know headwords, not inflected
;; forms; without it, words are looked up as typed.
(when (my/stardict-available-p)

  (defun my/greek-dict-lookup (word)
    "Show the entries for Greek WORD in every StarDict dictionary sdcv knows.
WORD may be inflected when Diogenes is installed: it reduces WORD to its
lemma first."
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
        (user-error "No entry for %s in the dictionaries under %s"
                    lemma (abbreviate-file-name my/stardict-dir)))
      ;; Show the buffer before filling it: HTML is wrapped to its window's width.
      (pop-to-buffer (get-buffer-create "*greek-dict*"))
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
        (goto-char (point-min)))))

  (defun my--greek-lemma (word)
    "Return the dictionary form of Greek WORD, as parsed by Diogenes.
Without Diogenes, return WORD unchanged."
    (if (not (my/diogenes-available-p))
        word
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
         (diogenes--perseus-ensure-utf8 (or lemma word) "greek")))))

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

  (keymap-global-set "C-c D" #'my/greek-dict-lookup))

(provide 'greek-study)
;;; greek-study.el ends here
