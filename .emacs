;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
(setq inhibit-startup-message t)
(setq show-paren-mode t)
(setq column-number-mode t)
(setq buffers-menu-max-size 100)
(setq c-basic-offset 4)
(setq c-default-style "stroustrup")
(setq fill-column 80)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
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
(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-install package)))

(use-package ivy
  :ensure t)

(use-package projectile
  :ensure t
  :bind ("M-p" . projectile-command-map)
  :config (progn
	    (projectile-mode +1)
	    (setq projectile-completion-system 'ivy)))
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
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))

(use-package magit
  :ensure t
  :config (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1))

(use-package smartparens
  :ensure t
  :config (progn
	    (require 'smartparens-config)
	    (smartparens-global-mode 1)))

(use-package base16-theme
  :ensure t
  :config (load-theme 'base16-material-darker t))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS X specific settings
(if (eq system-type 'darwin)
    (progn
      (use-package exec-path-from-shell
        :ensure t
        :hook (after-init . exec-path-from-shell-initialize))
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))
