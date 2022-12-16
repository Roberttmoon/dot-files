;;;;         ;;;;
;; emacs setup ;;
;;;;         ;;;;

(setq user-full-name "Robert Moon"
      user-mail-address "roberttmoon@gmail.com")

(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\")
			    (?\{ . ?\})))
(delete-selection-mode 1)

;;;;           ;;;;
;; package setup ;;
;;;;           ;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))

(package-initialize)
(defvar myPackages
  '(;; system packages
    use-package
    flycheck
    company
    auto-complete
    yasnippet
    exec-path-from-shell
    yaml-mode
    elmacro
    better-defaults
    ;; tools
    neotree
    elmacro
    restclient
    password-mode
    better-defaults
    pass
    ;; lsp stuff
    lsp-mode
    lsp-ui
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
    indium
    ;; typescript tools
    typescript-mode
    tide
    ;; go stuff
    go-mode
    go-fill-struct
    flycheck-golangci-lint
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

(when (memq window-system '(mac x))
  (setq exec-path (append "/usr/local/bin/aspell" exec-path)))

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

;;;;     ;;;;
;; go mode ;;
;;;;     ;;;;

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;;;;      ;;;;
;; lsp mode ;;
;;;;      ;;;;

(use-package lsp-mode
  :config
  (setq lsp-prefer-flymake nil ;; Prefer using lsp-ui (flycheck) over flymake.
        lsp-enable-xref t)

  (setq lsp-keymap-prefix "C-c C-l")

  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'javascript-mode #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp-deferred))

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
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

;;;;                  ;;;;
;; custom set varibales ;;
;;;;                  ;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("928ed6d4997ec3cdce10b65c59d0f966a61792a69b84c47155cb5578ce2972be" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" default))
 '(package-selected-packages
   '(k8s-mode kubernetes twilight-bright-theme php-mode go-eldoc company-jedi flycheck-golangci-lint flymake-golangci go-fill-struct restclient mu4e-alert mu4e-conversation mu4e-jump-to-list mu4e-maildirs-extension mu4e-overview mu4e-query-fragments powershell ejson-mode flycheck-demjsonlint json-mode json-navigator json-reformat jsonl magit clojure-mode flymake-lua company-lua lua-mode tide typescript-mode indium company-tern xref-js2 js2-refactor js2-mode dockerfile-mode terraform-mode gitlab-ci-mode heroku-theme better-defaults yasnippet yaml-mode use-package neotree lsp-ui lsp-treemacs flycheck exec-path-from-shell elmacro company-lsp auto-complete)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
