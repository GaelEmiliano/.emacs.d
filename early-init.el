;;; early-init.el --- A simple early-init file.

;;; Commentary:
;;; Simple early-init.el file.

;;; Code:
(setq package-enable-at-startup nil)

(menu-bar-mode -1)    ;; Disable the menu bar
(tool-bar-mode -1)    ;; Disable the tool bar
(scroll-bar-mode -1)  ;; Disable the scroll bar

(global-display-line-numbers-mode 1) ;; Display line numbers
(global-visual-line-mode t)          ;; Enable truncated lines

(display-time-mode 1)     ;; Displays the time
(display-battery-mode 1)  ;; Displays the battery

(global-hl-line-mode 1) ;; Enable showing a hl line

(setq column-number-mode t)  ;; Enable showing the column number
;;; early-init.el ends here
