(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ada-indent 1)
 '(auto-save-timeout 5)
 '(auto-save-visited-mode t)
 '(column-number-mode t)
 '(connection-local-criteria-alist
	 '(((:application tramp :protocol "kubernetes") tramp-kubernetes-connection-local-default-profile)
		 ((:application tramp :machine "localhost") tramp-connection-local-darwin-ps-profile)
		 ((:application tramp :machine "IB-MKARANASHEV-M") tramp-connection-local-darwin-ps-profile)
		 ((:application tramp) tramp-connection-local-default-system-profile
			tramp-connection-local-default-shell-profile)
		 ((:application eshell) eshell-connection-default-profile)))
 '(connection-local-profile-alist
	 '((tramp-kubernetes-connection-local-default-profile
			(tramp-config-check . tramp-kubernetes--current-context-data)
			(tramp-extra-expand-args 97 (tramp-kubernetes--container (car tramp-current-connection)) 104
															 (tramp-kubernetes--pod (car tramp-current-connection)) 120
															 (tramp-kubernetes--context-namespace (car tramp-current-connection))))
		 (tramp-connection-local-darwin-ps-profile
			(tramp-process-attributes-ps-args "-acxww" "-o"
																				"pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
																				"-o" "state=abcde" "-o"
																				"ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
			(tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string)
																					(egid . number) (comm . 52) (state . 5) (ppid . number)
																					(pgrp . number) (sess . number) (ttname . string)
																					(tpgid . number) (minflt . number) (majflt . number)
																					(time . tramp-ps-time) (pri . number) (nice . number)
																					(vsize . number) (rss . number) (etime . tramp-ps-time)
																					(pcpu . number) (pmem . number) (args)))
		 (tramp-connection-local-busybox-ps-profile
			(tramp-process-attributes-ps-args "-o"
																				"pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
																				"-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
			(tramp-process-attributes-ps-format (pid . number) (user . string) (group . string)
																					(comm . 52) (state . 5) (ppid . number) (pgrp . number)
																					(ttname . string) (time . tramp-ps-time) (nice . number)
																					(etime . tramp-ps-time) (args)))
		 (tramp-connection-local-bsd-ps-profile
			(tramp-process-attributes-ps-args "-acxww" "-o"
																				"pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
																				"-o"
																				"state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
			(tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string)
																					(egid . number) (group . string) (comm . 52)
																					(state . string) (ppid . number) (pgrp . number)
																					(sess . number) (ttname . string) (tpgid . number)
																					(minflt . number) (majflt . number) (time . tramp-ps-time)
																					(pri . number) (nice . number) (vsize . number)
																					(rss . number) (etime . number) (pcpu . number)
																					(pmem . number) (args)))
		 (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
																									 (shell-command-switch . "-c"))
		 (tramp-connection-local-default-system-profile (path-separator . ":")
																										(null-device . "/dev/null"))
		 (eshell-connection-default-profile (eshell-path-env-list))))
 '(custom-enabled-themes '(eink))
 '(custom-safe-themes
   '("9514e1fd8816c5a1f92e149936adc48ed5f9f19e33255099a13be0ee60cc675e"
     "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633"
     "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36"
     "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
     "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5"
     "36d4b9573ed57b3c53261cb517eef2353058b7cf95b957f691f5ad066933ae84"
     "972f792651d32b0506481b9e87b2fbc9b732ae9da2527562668c6e7d149fefda"
     "8896994441276c7ae32463776d5cbb71b51d8e3241b1b7c981306a3c4fbead07"
     "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
     "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659"
     "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "2b0fcc7cc9be4c09ec5c75405260a85e41691abb1ee28d29fcd5521e4fca575b"
     "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c"
     "84106b6b1ea9fe100a88ca60ee0a334fa20b8f80f6e81c362a0c0f709b01f2c2"
     "d6d4e0512dcaae663f7bd304557d6bc8b78c576be5af9c0b62b8447fb79b5fde"
     "021321ae56a45794f43b41de09fb2bfca184e196666b7d7ff59ea97ec2114559"
     "a6473f7abf949f4a6a1a9cc0dd37ea2e35ba3cea65d3442b98d65c5c5c5cb8d7"
     "a3e99dbdaa138996bb0c9c806bc3c3c6b4fd61d6973b946d750b555af8b7555b"
     "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" default))
 '(default-frame-alist '((vertical-scroll-bars) (undecorated . t)))
 '(display-line-numbers-type 'relative)
 '(eglot-confirm-server-edits nil nil nil "Customized with use-package eglot")
 '(global-display-line-numbers-mode t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((vertical-scroll-bars) (fullscreen . maximized)))
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'none)
 '(org-export-backends '(ascii html md odt))
 '(org-log-done 'time)
 '(org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE")))
 '(package-selected-packages
   '(0blayout avy basic-theme benchmark-init claude-code company-ledger eat eink-theme elpy erlang
              evil-ledger evil-org exec-path-from-shell find-file-in-project fuel fzf gptel
              haskell-mode helm json-mode lsp-mode magit marginalia mcp org-bullets org-gcal
              password-store persistent-scratch perspective phscroll pi-coding-agent projectile
              rustic slime solarized-theme sweeprolog timu-spacegrey-theme ultra-scroll undo-fu
              undo-tree verb vertico vterm zenburn-theme))
 '(package-vc-selected-packages
   '((phscroll :vc-backend Git :url "https://github.com/misohena/phscroll")
     (claude-code :url "https://github.com/stevemolitor/claude-code.el")))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-directories '("/Users/mkaranashev/.emacs.d/"))
 '(scroll-bar-mode nil)
 '(solarized-scale-org-headlines t)
 '(solarized-use-less-bold t)
 '(tab-width 8)
 '(tool-bar-mode nil)
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 140 :width normal :foundry "nil" :family "Iosevka SS04")))))
