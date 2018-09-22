;;;;         ;;;;
;; emacs setup ;;
;;;;         ;;;;

(setq user-full-name "Robert Moon"
      user-mail-address "robertmoon@northwesternmutual.com")

(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    heroku-theme
    flycheck
    neotree
    exec-path-from-shell
    yaml-mode
    elmacro
    ;; terraform tools
    terraform-mode
    hcl-mode
    ;; docker tools
    dockerfile-mode
    ;; python tools
    elpy
    ein
    py-autopep8))

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
(load-theme 'heroku t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(exec-path-from-shell-initialize)
(require 'neotree)
(global-set-key [f9] 'neotree-toggle)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (password-store restclient neotree heroku-theme heroku better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
