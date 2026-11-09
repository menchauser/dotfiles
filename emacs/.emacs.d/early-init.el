;;; early-init.el --- Settings that must precede GUI and package init -*- lexical-binding: t; -*-

;; Startup GC budget.  The restore hook is registered here rather than at the
;; end of init.el because errors in init.el are caught by startup.el: a hook
;; registered there would be skipped and GC would stay effectively off.
(setq gc-cons-threshold (* 128 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

;; The initial frame is created before init.el is read, so the tool bar mode
;; and the frame parameter alists have to be set here to affect that frame.
(tool-bar-mode -1)

(setq initial-frame-alist
      '((vertical-scroll-bars)
        (fullscreen . maximized)))

(setq default-frame-alist
      '((vertical-scroll-bars)
        (undecorated . t)))
