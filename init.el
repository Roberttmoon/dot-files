;;;;         ;;;;
;; emacs setup ;;
;;;;         ;;;;

(setq user-full-name "Robert Moon"
      user-mail-address "robertmoon@northwesternmutual.com")

(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\")
                            (?\{ . ?\})))

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    yasnippet
    heroku-theme
    flycheck
    neotree
    exec-path-from-shell
    yaml-mode
    elmacro
    company    
    auto-complete
    lsp-mode
    lsp-ui
    company-lsp
    lsp-treemacs
    dap-mode
    ;; gitlab-ci tools
    gitlab-ci-mode
    ;; terraform tools
    terraform-mode
    hcl-mode
    ;; docker tools
    dockerfile-mode
    ;; python tools
    elpy
    ein
    py-autopep8
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
    ;; go tools
    go-mode
    go-autocomplete
    company-go
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

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

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(exec-path-from-shell-initialize)
(setq-default indent-tabs-mode nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(package-selected-packages
   (quote
    (typescript-mode auto-complete-sage bash-completion company-shell chess cider clojure-mode threes flymake-shellcheck material-theme magit company-lua flymake-lua pass password-store restclient neotree heroku-theme heroku better-defaults)))
 '(tab-with 4 t))
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(yas-global-mode 1)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'neotree-show)

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

;;;;        ;;;;
;; shell mode ;;
;;;;        ;;;;
(add-hook 'sh-mode-hook 'flycheck-mode)

;;;;         ;;;;
;; python mode ;;
;;;;         ;;;;

(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

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

(flycheck-add-mode 'javascript-eslint 'js2-mode)


(require 'company)
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
			   (setq js2-basic-offset 2)
			   (tern-mode)
			   (company-mode)))
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

;;;;          ;;;;
;; emacs basics ;;
;;;;          ;;;;

(require 'gitlab-ci-mode)
(add-to-list 'auto-mode-alist '(".gitlab-ci.yml" . gitlab-ci-mode))

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

;;;;     ;;;;
;; go mode ;;
;;;;     ;;;;

;; (use-package lsp-mode
;;              :commands (lsp lsp-deferred))
;; (add-hook 'go-mode-hook #'lsp-deferred)
;; (use-package lsp-ui
;;              :commands lsp-ui-mode)
;; (use-package company-lsp
;;              :commands company-lsp)

;; (setq go-tab-width 4)
;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (when window-system (set-exec-path-from-shell-PATH))
;; (setenv "GOPATH" "/Users/moo7594/Development/gocode")

;; (defun my-go-mode-hook ()
;;   ; Use goimports instead of go-fmt
;;   (setq gofmt-command "goimports")
;;   ; Call Gofmt before saving
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   ; Customize compile command to run go build
;;   (if (not (string-match "go" compile-command))
;;       (set (make-local-variable 'compile-command)
;;            "go build -v && go test -v && go vet"))
;;   ; Godef jump key binding
;;   (local-set-key (kbd "M-.") 'godef-jump)
;;   (local-set-key (kbd "M-*") 'pop-tag-mark)
;;   (local-set-key (kbd "C-c C-f p") 'goprintf)
;;   (local-set-key (kbd "C-c C-f s") 'gosprintf)
;;   (local-set-key (kbd "C-c C-f e") 'goerrorf)
;;   (local-set-key (kbd "C-c e") 'goerr)
;;   (local-set-key (kbd "C-c o") 'gonotok))
;; (add-hook 'go-mode-hook 'my-go-mode-hook)

;; (defun goprintf ()
;;   (interactive)
;;   (insert "fmt.Printf(\"\\n\")")
;;   (backward-char 4))
;; (defun gosprintf ()
;;   (interactive)
;;   (insert "fmt.Sprintf(\"\\n\")")
;;   (backward-char 4))
;; (defun goerrorf ()
;;   (interactive)
;;   (insert "fmt.Errorf(\": %s\", err)")
;;   (backward-char 11))
;; (defun goerr ()
;;   (interactive)
;;   (insert "if err != nil {}")
;;   (backward-char 1)
;;   (newline nil 1)
;;   (insert "return nil, err")
;;   (next-line 1 1)
;;   (newline nil 1))
;; (defun gonotok ()
;;   (interactive)
;;   (insert "if !ok {}")
;;   (backward-char 1)
;;   (newline nil 1)
;;   (insert "return nil, fmt.Errorf(\"was not ok\\n\")")
;;   (next-line 1 1)
;;   (newline nil 1))

;;;;            ;;;;
;; terraform mode ;;
;;;;            ;;;;

;; (load-file "./.emacs.d/terraform-macros.el")
;; (defun tf-mode-hook ()
;;   (local-set-key (kbd "C-c e") 'tf-interpolation)
;;   (local-set-key (kbd "C-c v") 'tf-variable)
;;   (local-set-key (kbd "C-c o") 'tf-output)
;;   (local-set-key (kbd "C-c f") 'terraform-format-buffer))
;; (add-hook 'terraform-mode 'tf-mode-hook)

;;;;             ;;;;
;; better defaults ;;
;;;;             ;;;;


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
