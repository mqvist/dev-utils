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
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package) ; Bootstrap John Wigley's `use-package'
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish
  :ensure t)

(use-package ivy
  :ensure t
  :diminish)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package projectile
  :ensure t
  :bind-keymap ("M-p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy))

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

(use-package anaconda-mode
  :ensure t
  :diminish
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package blacken
  :ensure t
  :diminish
  :hook (python-mode . blacken-mode))

(use-package magit
  :ensure t
  :config (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1))

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

(use-package htmlize
  :ensure t)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package smartparens
  :ensure t
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-material-darker t)
  ;; Make comments slightly easier to see
  (set-face-foreground 'font-lock-comment-face "#707070"))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'arrow)
  (spaceline-spacemacs-theme))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode 1))

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package window
  :bind  (("S-C-<left>" . shrink-window-horizontally)
	  ("S-C-<right>" . enlarge-window-horizontally)
	  ("S-C-<down>" . shrink-window)
	  ("S-C-<up>" . enlarge-window)))

(use-package windmove
  :init (windmove-default-keybindings))

(use-package bs
  :bind ("C-x C-b" . bs-show))

(use-package eldoc
  :diminish)

(use-package abbrev
  :diminish)

(use-package subword
  :diminish
  :hook (prog-mode . subword-mode))

(use-package desktop
  :config (desktop-save-mode 1))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :hook (after-init . exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS X specific settings
(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))
