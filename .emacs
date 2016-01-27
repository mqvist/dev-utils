(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(global-set-key (kbd "C-M-g") 'goto-line)
(global-set-key (kbd "RET") 'newline-and-indent)
(windmove-default-keybindings)

;; Uncomment for fullscreen frame with some default splits
;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;(split-window-horizontally)
;(split-window-horizontally)
;(balance-windows)

(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(column-number-mode t)
'(font-use-system-font t)
'(fringe-mode (quote (0)) nil (fringe))
'(mouse-buffer-menu-maxlen 40)
'(mouse-buffer-menu-mode-mult 10)
'(show-paren-mode t)
'(tool-bar-mode nil)
'(tooltip-mode nil))
