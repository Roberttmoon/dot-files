;;;;         ;;;;
;; emacs setup ;;
;;;;         ;;;;

(setq user-full-name "Robert Moon"
      user-mail-address "roberttmoon@gmail.com")

(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\")
			    (?\{ . ?\})))

;;;;           ;;;;
;; package setup ;;
;;;;           ;;;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t) 

(package-initialize)
(defvar myPackages
  '(;; system packages
    use-package
    neotree
    company
    flycheck
    auto-complete
    yasnippet
    ;; lsp stuff
    lsp-mode
    lsp-ui
    company-lsp
    
    ;; themes!
    heroku-theme
    ))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

(when (not package-archive-contents)
  (package-refresh-contents))

;;;;            ;;;;
;; terminal mouse ;;
;;;;            ;;;;

(require 'xt-mouse)
(xterm-mouse-mode)
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))

(setq mouse-wheel-follow-mouse 't)

(defvar alternating-scroll-down-next t)
(defvar alternating-scroll-up-next t)

(defun alternating-scroll-down-line ()
  (interactive "@")
    (when alternating-scroll-down-next
;      (run-hook-with-args 'window-scroll-functions )
      (scroll-down-line))
    (setq alternating-scroll-down-next (not alternating-scroll-down-next)))

(defun alternating-scroll-up-line ()
  (interactive "@")
    (when alternating-scroll-up-next
;      (run-hook-with-args 'window-scroll-functions)
      (scroll-up-line))
    (setq alternating-scroll-up-next (not alternating-scroll-up-next)))

(global-set-key (kbd "<mouse-4>") 'alternating-scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'alternating-scroll-up-line)

;;;;          ;;;;
;; emacs basics ;;
;;;;          ;;;;

(load-theme 'heroku t) ;; load material theme

(setq inhibit-startup-message t) ;; hide the startup message
(global-linum-mode t) ;; enable line numbers globally
(setq-default indent-tabs-mode nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (heroku-theme lsp-ui company-lsp use-package neotree lsp-mode flycheck company auto-complete)))
 '(tab-with 4 t))
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'neotree-show)

(when (memq window-system '(mac x))
  (exec-path-from-shell-initialize))

;; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode)
            (local-set-key (kbd "C-c b") 'eval-buffer)
            (local-set-key (kbd "C-c r") 'eval-region)))

;;;;          ;;;;
;; elmacro mode ;;
;;;;          ;;;;

(require 'elmacro)
(elmacro-mode)

;;;;       ;;;;
;; ymal mode ;;
;;;;       ;;;;

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;;;      ;;;;
;; lsp mode ;;
;;;;      ;;;;

(use-package lsp-mode
  :config

  (setq lsp-prefer-flymake nil ;; Prefer using lsp-ui (flycheck) over flymake.
        lsp-enable-xref t)

  (add-hook 'python-mode-hook #'lsp))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :config

  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)

  ;; Remap keys for xref find defs to use the LSP UI peek mode.
  ;;(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company
  :config
  (setq company-idle-delay 0.3)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)

   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
