;;; init.el --- my init file
;;; Commentary:
;;; Frankenstein 2.0
;;; Code:

;; start package.el with Emacs
(require 'package)

;; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; initialize package.el
(package-initialize)

;; unless there is cache, update the package metadata
(unless package-archive-contents
  (package-refresh-contents))

;; reduce the amount of time spent garbage collecting, but increase total memory use
;; increase the threshold from the default 800KB to 37MB
(setq gc-cons-threshold (* 37 1000 1000))

;; always load newest byte code
(setq load-prefer-newer t)

;; stop emacs adding customised settings in the init file
;; creating it as a temporary file effectively disables it
(setq custom-file (make-temp-file "emacs-custom"))

;; clear the screen
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)


;; load in fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; don't display the help screen on startup
(setq inhibit-startup-screen t)

;; no tabs
(setq-default indent-tabs-mode nil)

;; prettify symbols
(global-prettify-symbols-mode +1)

;; smooth emaaacs scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

;; newline at the end of the file
(setq require-final-newline t)

;; lines upto 80 characters
(setq-default fill-column 80)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; highlight the other parenthesis of the pair
(show-paren-mode t)
(setq show-paren-delay 0)

;; copy shell path
(defun set-exec-path-from-shell-PATH ()
  "Copy the shell path since I start Emacs from the dock."
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$"
          ""
          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

;;  open "*.rkt" files in racket-mode
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

;;  open "*.scm" files in racket-mode
(add-to-list 'auto-mode-alist '("\\.scm\\'" . racket-mode))

;; use-package: A use-package declaration for simplifying your .emacs
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;; get information for package loads
(setq use-package-verbose t)
;; equivalent to setting ":ensure t" on each call to use-package
(setq use-package-always-ensure t)

;; show line number
;;(global-display-line-numbers-mode t)
(use-package nlinum-relative
  :config
  ;; something else you want
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (setq nlinum-relative-current-symbol ""))

;; evil: extensible vi layer for Emacs
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

;; evil-collection: A set of keybindings for evil-mode
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; evil-escape: Customizable key sequence to escape from insert state and everything else in Emacs
(use-package evil-escape
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk"))

;; magit: A Git Porcelain inside Emacs
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . 'magit-dispatch))
  :config
  (setq magit-view-git-manual-method 'man))

;; evil-magit: evil keys for magit
(use-package evil-magit
  :after magit)

;; zenburn-theme: The Zenburn colour theme ported to Emacs
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; iedit: Edit multiple regions in the same way simultaneously
(use-package iedit
  :bind (("C-;" . iedit-mode)))

;; ix: simple emacs client to http://ix.io cmdline pastebin
(use-package ix)

;; company: ext completion framework for Emacs
(use-package company
  :config
  ;; the old config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 1)
  ;; Use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  (company-tng-mode)

  (setq-local completion-ignore-case t)

  ;; paths are added in custom
  (add-to-list 'company-backends 'company-c-headers))


;; company-c-headers: Auto-completion for C/C++ headers using Company
(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers)
  ;; list generated with: gcc -xc++ -E -v -
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/../../../../include/c++/10.2.0")
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/../../../../include/c++/10.2.0/x86_64-pc-linux-gnu")
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/../../../../include/c++/10.2.0/backward")
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include")
  (add-to-list 'company-c-headers-path-system "/usr/local/include")
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include-fixed")
  (add-to-list 'company-c-headers-path-system "/usr/include"))

;; all cc-mode related stuff
(use-package cc-mode
  :config
  ;; c++ public/protected/private identation
;;; http://stackoverflow.com/questions/4490196/emacs-public-protected-private-label-indentation-of-c-header-file-not-working
  (c-set-offset 'access-label -1)

  ;; c++ switch indentation
  ;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Indenting-switch-statements.html
  (c-set-offset 'case-label '+)

  ;; quick switch between header and implementation
  (add-hook 'c-mode-common-hook (lambda() (local-set-key (kbd "C-c o") 'ff-find-other-file)))

  ;; this solved jumping pairs problem
  ;; https://superuser.com/questions/255510/how-to-toggle-between-cpp-and-hpp-that-are-not-in-the-same-directory
  (setq cc-other-file-alist
        '(("\\.c"   (".h"))
          ("\\.cpp"   (".h"))
          ("\\.h"   (".cpp"".c"))))
  (setq ff-search-directories
        '("." "../src" "../include"))

  ;; Open .h files in cpp mode.
  ;; https://stackoverflow.com/questions/3312114/how-to-tell-emacs-to-open-h-file-in-c-mode
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

(use-package semantic
  :config
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style))

;; flycheck: Syntax checking for GNU Emacs
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; flycheck-cpplint: adds cpplint to flycheck checkers
(use-package flycheck-cpplint
  :ensure nil
  :load-path "local/flycheck-cpplint")

;; yasnippet: A template system for Emacs
(use-package yasnippet
  :config
  (yas-global-mode 1)
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  )


(use-package yasnippet-snippets)
;; ranger:
(use-package ranger
  :config
  (ranger-override-dired-mode t))

;; ;; autopair: Automagically pair braces and quotes in emacs like TextMate
(use-package autopair
  :config
  (autopair-global-mode))


;; highlight-parentheses: highlight surrounding parentheses
(use-package highlight-parentheses
  ;; just use the old and tested config for now
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode
    (lambda ()
      (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t))


;; faster recompile
(global-set-key [f5] 'recompile)

;; save all buffers for me before compilation
(setq compilation-ask-about-save nil)

(add-hook 'compilation-finish-functions #'my-compile)
(defun my-compile (buf str)
  "Open shell in the compilation window if compilation exits normally.
See 'compilation-finish-functions to for the arguments:  BUF STR."
  (if (null (string-match ".*exited abnormally.*" str))
      (progn
        (shell)
        (message "No Compilation Errors!"))))


;; format the file before saving
(add-hook 'before-save-hook #'indent-buffer)

;; https://stackoverflow.com/questions/4090793/emacs-reindenting-entire-c-buffer
(defun indent-buffer ()
  "Indent the entire buffer using the default indenting scheme."
  (interactive)
  ;; when in the listed modes do your thing
  (when (derived-mode-p 'cc-mode 'c++-mode 'emacs-lisp-mode)
    (save-excursion
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max) nil)
      (untabify (point-min) (point-max)))))


;;; init.el ends here
