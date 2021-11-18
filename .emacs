;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
(setq inhibit-startup-message t)
(setq column-number-mode t)
(setq buffers-menu-max-size 100)
;; Force using 4 spaces for indentation in all modes by default
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq c-default-style "stroustrup")
(setq-default fill-column 80)
(c-set-offset 'innamespace 0)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)
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

(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package
(unless (package-installed-p 'use-package) ; Bootstrap John Wigley's `use-package'
  (package-install 'use-package))

(require 'use-package)

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
  :custom
  ;; Switch crux to use vterm. In order for crux to reuse an existing terminal,
  ;; its name has to have asterisks around it.
  (crux-term-func (lambda (arg) (vterm (concat "*" arg "*"))))
  (crux-term-buffer-name "vterm")
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

(use-package org
  :init
  (setq org-hide-leading-stars t)
  (setq org-cycle-separator-lines -1)
  :config
  (progn
    (setq org-agenda-files '("~/google_drive/org/todo.org"))
    (setq org-archive-location "~/google_drive/org/journal.org::datetree/")
    (setq org-capture-templates
          '(("t" "TODO" entry
             (file+headline "~/google_drive/org/todo.org" "Tasks")
             "* TODO %^{Description} %^g\n  %?\n  Added: %U"
             :empty-lines 1)
            ("T" "TODO today" entry
             (file+headline "~/google_drive/org/todo.org" "Tasks")
             "* TODO %^{Description} %^g\n  SCHEDULED: %t\n  %?\n  Added: %U"
             :empty-lines 1)
            ("j" "Journal entry" entry
             (file+datetree "~/google_drive/org/journal.org")
             "* %^{Start time|%<%k:%M>} %^{Title} %^g\n  %?"
             :empty-lines 1)))
    (setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-log-done 'note)
    (setq org-agenda-show-future-repeats 'next)
    ;; Disable some keybinding that conflict with the window keys
    (unbind-key "C-S-<up>" org-mode-map)
    (unbind-key "C-S-<down>" org-mode-map)
    (unbind-key "C-S-<up>" org-mode-map)
    (unbind-key "C-S-<down>" org-mode-map))
  :hook ((org-shiftup-final . windmove-up)
         (org-shiftleft-final . windmove-left)
         (org-shiftdown-final . windmove-down)
         (org-shiftright-final . windmove-right)
         (org-mode . turn-on-auto-fill)
         ;; Disable some keybinding that conflict with the windmove keys
         (org-agenda-mode . (lambda ()
                              (progn
                                (unbind-key "S-<up>" org-agenda-mode-map)
                                (unbind-key "S-<down>" org-agenda-mode-map)))))
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link)
         ("C-c c" . org-capture)))

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package all-the-icons
  :ensure t)

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

;; (use-package elm-mode
;;   :ensure t
;;   :hook (elm-mode . elm-format-on-save-mode))

(use-package haskell-mode
  :ensure t
  :custom
  (haskell-tags-on-save t)
  (haskell-process-type 'stack-ghci)
  (haskell-stylish-on-save t)
  (haskell-mode-stylish-haskell-path "brittany")
  :hook
  (haskell-mode . interactive-haskell-mode)
  :bind (("M-." . haskell-mode-jump-to-tag)
	 ("C-c C-d" . haskell-hoogle)
	 ("M-n" . haskell-goto-next-error)
	 ("C-M-n" . haskell-goto-prev-error)
	 ("C-c l" . haskell-check)))

(use-package htmlize
  :ensure t)

(use-package magit
  :ensure t
  :config (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  :bind (("C-x g" . magit-status)))

;; (use-package racket-mode
;;   :ensure t)

;; (use-package racket-xp
;;   :diminish
;;   :hook (racket-mode . racket-xp-mode)
;;   :hook (racket-xp-mode . (lambda ()
;; 			    (remove-hook 'pre-redisplay-functions
;; 					 #'racket-xp-pre-redisplay
;; 					 t))))

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
