;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq buffers-menu-max-size 100)
(setq c-basic-offset 4)
(setq c-default-style "stroustrup")
(setq-default fill-column 80)
(c-set-offset 'innamespace 0)
(setq make-backup-files nil) ; stop creating backup~ files
;; (setq auto-save-default nil) ; stop creating #autosave# files
(setq require-final-newline t)
;; Prevent GConf from causing Emacs font changes
(define-key special-event-map [config-changed-event] 'ignore)
(show-paren-mode)
(global-auto-revert-mode 1)
(fringe-mode '(0 . 8))
(tool-bar-mode -1)
;; This has to be at the top so any new packages are registered to
;; package-selected-packages correctly.
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configuration
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package) ; Bootstrap John Wigley's `use-package'
  (package-refresh-contents)
  (package-install 'use-package))

;; General packages and settings
(use-package abbrev
  :diminish)

(use-package ansi-color
  :init
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  :hook (compilation-filter . colorize-compilation-buffer))

(use-package term
  :hook (term-mode .  (lambda () (set-face-attribute 'comint-highlight-prompt nil
						     :inherit nil)))
  :custom-face
  (term ((t (:background "#101010" :foreground "gray")))))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-material-darker t)
  ;; Make background darker
  (set-background-color "#101010")
  ;; Make comments slightly easier to see
  (set-face-foreground 'font-lock-comment-face "#707070"))

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-blink-when-focused 1)
  (setq beacon-blink-when-window-changes 1)
  (setq beacon-blink-when-buffer-changes 1))

(use-package bs
  :bind ("C-x C-b" . bs-show))

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c s" . crux-transpose-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-^" . crux-top-join-line)
         ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package desktop
  :config (desktop-save-mode 1))

(use-package diminish
  :ensure t)

(use-package ivy
  :ensure t
  :diminish)

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package org-mode
  :init
  (setq org-hide-leading-stars t)
  (setq org-cycle-separator-lines -1)
  :config
  (setq org-support-shift-select 'always)
  :hook ((org-shiftup-final . windmove-up)
	 (org-shiftleft-final . windmove-left)
	 (org-shiftdown-final . windmove-down)
	 (org-shiftright-final . windmove-right)
	 (org-mode . turn-on-auto-fill))
  :bind (("C-c a" . org-agenda)
	 ("C-c l" . org-store-link)))

(use-package projectile
  :ensure t
  :bind-keymap ("M-p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy))

(use-package smartparens
  :ensure t
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  :bind (("C-<delete>" . sp-unwrap-sexp)))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'arrow)
  (spaceline-spacemacs-theme))

(use-package subword
  :diminish
  :hook (prog-mode . subword-mode))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode 1))

(use-package window
  :bind  (("S-C-<left>" . shrink-window-horizontally)
	  ("S-C-<right>" . enlarge-window-horizontally)
	  ("S-C-<down>" . shrink-window)
	  ("S-C-<up>" . enlarge-window)))

(use-package windmove
  :init (windmove-default-keybindings))

;; Programming packages
(use-package anaconda-mode
  :ensure t
  :diminish
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package blacken
  :ensure t
  :diminish
  :hook (python-mode . blacken-mode))

(use-package eldoc
  :diminish)

(use-package elm-mode
  :ensure t
  :hook (elm-mode . elm-format-on-save-mode))

(use-package haskell-mode
  :ensure t
  :hook (haskell-mode . interactive-haskell-mode))

(use-package htmlize
  :ensure t)

(use-package magit
  :ensure t
  :config (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  :bind (("C-x g" . magit-status)))

(use-package quickrun
  :ensure t
  :bind (("C-c C-q" . quickrun)))

(use-package racket-mode
  :ensure t)

(use-package racket-xp
  :diminish
  :hook (racket-mode . racket-xp-mode)
  :hook (racket-xp-mode . (lambda ()
			    (remove-hook 'pre-redisplay-functions
					 #'racket-xp-pre-redisplay
					 t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages that are not supported in windows
(if (not (eq system-type 'windows-nt))
    (use-package vterm
      :ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS X specific settings
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :hook (after-init . exec-path-from-shell-initialize)))

(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))
