;;;;         ;;;;
;; emacs setup ;;
;;;;         ;;;;

(setq user-full-name "Robert Moon"
      user-mail-address "roberttmoon@gmail.com")

(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\")
			    (?\{ . ?\})))

(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

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
<<<<<<< HEAD
    neotree
=======
>>>>>>> 2e019ffbad2352921ab9542559163d9d3af2a37d
    flycheck
    company
    auto-complete
    yasnippet
    exec-path-from-shell
    yaml-mode
<<<<<<< HEAD
    elmacro
    better-defaults
=======
    ;; tools
    neotree
    elmacro
    better-defaults
    pass
>>>>>>> 2e019ffbad2352921ab9542559163d9d3af2a37d
    ;; lsp stuff
    lsp-mode
    lsp-ui
    company-lsp
    lsp-treemacs
    ;; themes!
    heroku-theme
    material-theme
    ;; gitlab-ci tools
    gitlab-ci-mode
    ;; terraform tools
    terraform-mode
    hcl-mode
    ;; docker tools
    dockerfile-mode
    ;; javascript tools
    js2-mode
    js2-refactor
    xref-js2
    company-tern
    indium
    ;; typescript tools
    typescript-mode
    tide
    ;; lua tools
    lua-mode
    company-lua
    flymake-lua
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
(load-theme 'material t) ;; load material theme

(setq inhibit-startup-message t) ;; hide the startup message

(global-linum-mode t) ;; enable line numbers globally
(setq-default indent-tabs-mode nil)


(yas-global-mode 1)

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
(require 'gitlab-ci-mode)
(add-to-list 'auto-mode-alist '(".gitlab-ci.yml" . gitlab-ci-mode))

;;;;        ;;;;
;; shell mode ;;
;;;;        ;;;;
(add-hook 'sh-mode-hook 'flycheck-mode)


;;;;             ;;;;
;; javascript mode ;;
;;;;             ;;;;

(require 'js2-mode)
 
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
			   (set-variable 'js2-strict-missing-semi-warning nil)
			   (set-variable 'indent-tabs-mode nil)
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; (flycheck-add-mode 'javascript-eslint 'js2-mode)

(require 'company)
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
			   (setq js2-basic-offset 2)
			   (tern-mode)
			   (company-mode)))
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;;;;             ;;;;
;; typescript mode ;;
;;;;             ;;;;

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))


(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)


;;;;      ;;;;
;; lua mode ;;
;;;;      ;;;;


(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(require 'company-lua)
(require 'flycheck)
(add-hook 'lua-mode-hook 'flycheck-mode)
(add-hook 'lua-mode-hook (lambda ()
                           (company-lua)))

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

;;;;                  ;;;;
;; custom set varibales ;;
;;;;                  ;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flymake-lua company-lua lua-mode tide typescript-mode indium company-tern xref-js2 js2-refactor js2-mode dockerfile-mode terraform-mode gitlab-ci-mode heroku-theme better-defaults yasnippet yaml-mode use-package neotree lsp-ui lsp-treemacs flycheck exec-path-from-shell elmacro company-lsp auto-complete))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
