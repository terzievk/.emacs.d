;;; init.el --- my init file
;;; Commentary:
;;; Frankenstein 2.0
;;;  This is the second version of my old config, adapted to use-package
;;; and  mixed in with things I liked mainly from the following:
;;;   https://github.com/bbatsov/emacs.d
;;;   https://github.com/triffon/emacs-config
;;;   https://tuhdo.github.io/index.html
;;;   https://github.com/jamiecollinson/dotfiles/blob/master/config.org/

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

;; stop cursor from blinking because it's annoying when switching windows
(blink-cursor-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)


;; load in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))


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

;; use-package: A use-package declaration for simplifying your .emacs
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;; get information for package loads
(setq use-package-verbose t)
;; equivalent to setting ":ensure t" on each call to use-package
(setq use-package-always-ensure t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

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

;; ;; helm: Emacs incremental completion and selection narrowing framework
;; (use-package helm
;;   :config
;;   ;;; extended config from: https://tuhdo.github.io/helm-intro.html

;;   ;; helm-config replaced with helm-autoloads
;;   ;;  https://github.com/emacs-helm/helm/commit/e81fbbc687705595ab65ae5cd3bdf93c17a90743

;;   ;;  (require 'helm-config)
;;   (require 'helm-autoloads)

;;   ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;;   ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;;   ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;   (global-set-key (kbd "C-c h") 'helm-command-prefix)
;;   (global-unset-key (kbd "C-x c"))

;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;;   (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
;;   (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;;   (when (executable-find "curl")
;;     (setq helm-google-suggest-use-curl-p t))

;;   (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
;;         helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
;;         helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
;;         helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
;;         helm-ff-file-name-history-use-recentf t
;;         helm-echo-input-in-header-line t)

;;   (defun spacemacs//helm-hide-minibuffer-maybe ()
;;     "Hide minibuffer in Helm session if we use the header line as input field."
;;     (when (with-helm-buffer helm-echo-input-in-header-line)
;;       (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;         (overlay-put ov 'window (selected-window))
;;         (overlay-put ov 'face
;;                      (let ((bg-color (face-background 'default nil)))
;;                        `(:background ,bg-color :foreground ,bg-color)))
;;         (setq-local cursor-type nil))))


;;   (add-hook 'helm-minibuffer-set-up-hook
;;             'spacemacs//helm-hide-minibuffer-maybe)

;;   (setq helm-autoresize-max-height 0)
;;   (setq helm-autoresize-min-height 20)
;;   (helm-autoresize-mode 1)

;;   (helm-mode 1))

;; zenburn-theme: The Zenburn colour theme ported to Emacs
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; iedit: Edit multiple regions in the same way simultaneously
(use-package iedit
  :bind (("C-;" . iedit-mode)))

;; sudo-edit: Utilities for opening files with sudo
(require 'sudo-edit)

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

  (setq-local completion-ignore-case t))


;; company-c-headers: Auto-completion for C/C++ headers using Company
(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers)
  ;; list generated with: gcc -xc++ -E -v -
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/12.2.0/../../../../include/c++/12.2.0")
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/12.2.0/../../../../include/c++/12.2.0/x86_64-pc-linux-gnu")
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/12.2.0/../../../../include/c++/12.2.0/backward")
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/12.2.0/include")
  (add-to-list 'company-c-headers-path-system "/usr/local/include")
  (add-to-list 'company-c-headers-path-system "/usr/lib/gcc/x86_64-pc-linux-gnu/12.2.0/include-fixed")

  (add-to-list 'company-c-headers-path-system "/usr/include")
  )

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
        '("." "../sources" "../includes" "/usr/include")) ;; little hack
  ;; for C-c o to work with stl, bound to fail on next update

  ;; Open .h files in cpp mode.
  ;; https://stackoverflow.com/questions/3312114/how-to-tell-emacs-to-open-h-file-in-c-mode
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

;; (use-package semantic
;;   :config
;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-scheduler-mode 1)
;;   (semantic-mode 1))

;; irony
(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(use-package google-c-style
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style))

;; flycheck: Syntax checking for GNU Emacs
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  ;; add cpplint to flycheck
  (add-to-list 'flycheck-checkers 'cpplint)
  (flycheck-define-checker cpplint
    "Google C/C++ style guide static syntax checker.

See URL `https://github.com/cpplint/cpplint'."
    ;; ignore header guard warnings since flycheck uses temp files
    ;; source-original is useless - needs a save after each edit to update flycheck
    :command ("cpplint" "--filter=-build/header_guard" source)
    :error-patterns
    ((error line-start (file-name) ":" line ":" (message) line-end))
    :modes (c-mode c++-mode)))

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

;; c++ default compile command
(add-hook 'c++-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   ;; name execs f for ease of use
                   (concat "g++ "
                           buffer-file-name
                           " -o f"
                           " -std=c++2b"
                           ;; some learncpp.com suggested options
                           " -O2 -DNDEBUG"
                           " -pedantic-errors"
                           " -Wall -Weffc++ -Wextra -Wconversion -Wsign-conversion"
                           " -Werror")))))
;; c default compile command
(add-hook 'c-mode-hook
          (lambda ()
            (unless (or (file-exists-p "makefile")
                        (file-exists-p "Makefile"))
              (set (make-local-variable 'compile-command)
                   ;; name execs f for ease of use
                   (concat "g++ "
                           buffer-file-name
                           " -o f")))))



;; save all buffers for me before compilation
(setq compilation-ask-about-save nil)

(add-hook 'compilation-finish-functions #'my-compile)
(defun my-compile (buf str)
  "Open shell in the compilation window if compilation exits normally.
See 'compilation-finish-functions to for the arguments:  BUF STR."
  (if (null (string-match ".*exited abnormally.*" str))
      (progn
        ;; (kill-buffer (process-buffer (get-process "shell")))
        (switch-to-buffer-other-frame buf)
        (shell)
        (message "No Compilation Errors!"))))


;; format the file before saving
(add-hook 'before-save-hook #'indent-buffer)

;; https://stackoverflow.com/questions/4090793/emacs-reindenting-entire-c-buffer
(defun indent-buffer ()
  "Indent the entire buffer using the default indenting scheme."
  (interactive)
  ;; when in the listed modes do your thing
  (when (derived-mode-p 'cc-mode 'c++-mode 'emacs-lisp-mode 'web-mode 'racket-mode
                        'css-mode 'js-mode)
    (save-excursion
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max) nil)
      (untabify (point-min) (point-max)))))

;; lsp
;; https://emacs-lsp.github.io/lsp-mode/page/installation/
(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js-mode . lsp)
         (php-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         ;; https://emacs-lsp.github.io/lsp-haskell/
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp))
  :commands lsp)

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (defun my-go-mode-hook ()
    "Some dodgy stuff inspired by http://tleyden.github.io/blog/2014/12/02/getting-started-with-go-and-protocol-buffers/"
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             ;; most executables are called f for ease of use
             "go generate && go build -v -o f && go test -v && go vet"))
    ;; Godef jump key binding
    (global-set-key (kbd "M-.") 'godef-jump)
    (global-set-key (kbd "M-*") 'pop-tag-mark)
    )

  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

;; dumb-jump
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package markdown-preview-eww)


;; r
(use-package ess
  :init (require 'ess-r-mode))

;; haskell
(use-package haskell-mode
  :config
  ;; (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
  ;; http://haskell.github.io/haskell-mode/manual/latest/Interactive-Haskell.html#Interactive-Haskell
  (define-key haskell-mode-map (kbd "<f5>") 'haskell-process-load-or-reload))

;; polymode R markdown -- not yet set up
(use-package poly-markdown)


;; python
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred

(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

;; ace-window Quickly switch windows in Emacs
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; Frankenstein 1.0 relic
;; my first not copy-paste emacs customization
;; save and open in browser the current file
(defun my-save-and-open-in-browser ()
  "Save the current buffer and open it in the default browser."
  (interactive)
  (save-buffer)
  (buffer-name)
  ;; little hack to match current web use case:
  ;; if the file is opened in web mode and isn't a *.html
  ;; find the single html in the parent directory and open it
  ;; NB! assumes there exist only 1 such html

  ;; (browse-url-of-file))
  (let* ((my-current-buffer (file-name-nondirectory buffer-file-name))
         ;; (my-current-html "index.html")
         (my-parent-directory
          (file-name-directory
           (directory-file-name
            (file-name-directory buffer-file-name))))
         (my-current-html
          (car (directory-files my-parent-directory t "\\.html?\\'"))))
    (if (string= "html" (file-name-extension buffer-file-name))
        (browse-url-of-file)
      (progn
        (find-file my-current-html)
        ;;(save-buffer)  ;; I don't think I'll need this but idk...
        (browse-url-of-file)
        (switch-to-buffer my-current-buffer)))))

;; web-mode
(use-package web-mode
  :ensure t
  ;; :custom
  ;; (web-mode-markup-indent-offset 2)
  ;; (web-mode-css-indent-offset 2)
  ;; (web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  ;; to do: read some elisp regex...
  ;;  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

  (eval-after-load 'web-mode
    '(define-key web-mode-map [f5] 'my-save-and-open-in-browser))
  )

;; css-mode
(use-package css-mode
  :config
  (eval-after-load 'css-mode
    '(define-key css-mode-map [f5] 'my-save-and-open-in-browser)))


;; js
;; don't forget to install npm:
;; https://archlinux.org/packages/community/x86_64/nodejs/
;; lsp:
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
;; some help on usage:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/reactjs-tutorial/
(with-eval-after-load 'js
  (define-key js-mode-map [f5] 'my-save-and-open-in-browser))

;; this was moved to: (use-package lsp-mode)
;; see: https://emacs-lsp.github.io/lsp-mode/page/installation/
;; (add-hook 'js-mode-hook #'lsp)

(setq js-indent-level 2)

;; racket
(use-package racket-mode
  :ensure t
  :config
  ;;  open "*.rkt" files in racket-mode
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  ;; f5 instead of C-c C-c
  (add-hook 'racket-mode-hook
            (lambda ()
              (define-key racket-mode-map (kbd "<f5>") 'racket-run)))
  )

;; yaml-mode
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; php
(use-package php-mode
  :ensure t
  :mode
  ("\\.php\\'" . php-mode)
  :config
  (define-key php-mode-map (kbd "<f5>") 'my-temp))

;;; ain't nobody got time to type
(defun my-temp ()
  "Temp shortcut for current projest.
Save, allign and open `localhost/project`."
  (interactive)
  (lsp-format-buffer)
  (save-buffer)
  (browse-url-chrome "127.0.0.1"))

;; swift: https://github.com/emacs-lsp/lsp-sourcekit
;; competion doesn't work, see:  https://github.com/emacs-lsp/lsp-mode/issues/3028
(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/usr/lib/swift/bin/sourcekit-lsp"))

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

;;example erc config: https://codeberg.org/jao/elibs/src/branch/main/attic/misc.org
(use-package erc
  :init (setq erc-server "irc.libera.chat"))


;; https://www.emacswiki.org/emacs/EmacsAsDaemon#:~:text=The%20simplest%20way%20to%20stop,the%20associated%20emacs%20server%20instance.
;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )
;;; init.el ends here

;;;; agda
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
