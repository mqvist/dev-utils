;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configuration
(package-initialize)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-install package)))

;; Install some packages if needed
(use-package crux
  :ensure t)
(use-package anaconda-mode
  :ensure t)
(use-package blacken
  :ensure t)
(use-package magit
  :ensure t)
(use-package exec-path-from-shell
  :ensure t)
(use-package base16-theme
  :ensure t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS X specific settings
(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)
      (add-hook 'after-init-hook 'exec-path-from-shell-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key settings
(windmove-default-keybindings)
(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key [(control shift return)] #'crux-smart-open-line-above)
(global-set-key (kbd "C-c t") #'crux-visit-term-buffer)
(global-set-key (kbd "C-c s") #'crux-transpose-windows)
(global-set-key (kbd "C-c D") #'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d") #'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)
(global-set-key (kbd "C-c k") #'crux-kill-other-buffers)
(global-set-key (kbd "C-^") #'crux-top-join-line)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python-specific settings
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'blacken-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other settings
(setq inhibit-startup-message t)
(global-auto-revert-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs' area
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying-when-privileged-mismatch nil)
 '(buffers-menu-max-size 100)
 '(c-basic-offset 4)
 '(c-default-style "stroustrup")
 '(calendar-week-start-day 1)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-read-command nil)
 '(compilation-scroll-output t)
 '(compilation-window-height 10)
 '(compile-auto-highlight nil)
 '(custom-safe-themes
   (quote
    ("196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" default)))
 '(fill-column 80)
 '(fringe-mode (quote (0)) nil (fringe))
 '(indent-tabs-mode nil)
 '(kept-new-versions 0)
 '(kept-old-versions 0)
 '(lazy-lock-defer-time nil)
 '(lazy-lock-stealth-nice 0.0)
 '(lazy-lock-stealth-time nil)
 '(make-backup-files nil)
 '(mouse-buffer-menu-mode-mult 100)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 1) ((control)))))
 '(package-selected-packages
   (quote
    (use-package crux anaconda-mode magit blacken exec-path-from-shell base16-theme haskell-mode clojure-mode)))
 '(show-paren-mode t)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(version-control (quote never)))

;; Load theme
(load-theme 'base16-material-darker)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
