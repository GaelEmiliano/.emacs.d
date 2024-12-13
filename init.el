;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; This is the main configuration file for Emacs (init.el) where
;; packages are loaded and personal preferences are defined.

;------------------------------------;
;  Gael Emiliano Arreguín Salgado    ;
;------------------------------------;

;;; Code:

;; ---------------------------------
;; BASIC INTERFACE SETTINGS
;; ---------------------------------

;; Disable the startup screen
(setq inhibit-startup-screen t)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable tooltip
(tooltip-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bar
(scroll-bar-mode -1)

;; Activate full screen
;;(toggle-frame-fullscreen)

;; Disable margins
;;(set-window-fringes nil 0 0)

;; Show line numbers on the left side
(global-display-line-numbers-mode 1)

;; Use relative line numbers for better navigation
(setq display-line-numbers-type 'relative)

;; Time and date
(setq display-time-day-and-date 1)

;; Show the time
(display-time-mode 1)

;; Show battery percentage
(display-battery-mode 1)

;; Habilita cambiar de ventana usando Shift + flechas
(windmove-default-keybindings)

;; No crea archivos de respaldo
(setq make-backup-files nil)

;; No crea archivos de autoguardado
(setq auto-save-default nil)

;; Utiliza una señal visual para la campana
;; en lugar de un sonido
(setq visible-bell t)

;; Habilita modo de número de columna en la
;; barra de estado
(setq column-number-mode t)

;; Usa espacios en lugar de tabs para la indentación
(setq-default indent-tabs-mode nil)

;; Desactiva el registro de mensajes en el buffer *Messages*
(setq-default message-log-max nil)
;; Cierra o mata el buffer *Messages* si está abierto
(kill-buffer "*Messages*")

;; ---------------------------------
;; PACKAGE MANAGEMENT
;; ---------------------------------

;; Set up package repositories (MELPA and ELPA)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
	                 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Refresh package list if necessary
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if it's not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always ensure use-package installs missings packages
(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------------------------
;; APPEARANCE
;; ---------------------------------

;; --- Cursor ---
(setq-default cursor-type 'bar)

;; --- Themes ---

;; Dracula Theme
(use-package dracula-theme
  :ensure t
  :init
  (setq dracula-theme t)
  (load-theme 'dracula t))

;; Zenburn Theme
(use-package zenburn-theme
  :ensure t)

;; --- Font ---

;; Set font to "DejaVu Sans Mono, size 11"
(set-face-attribute 'default nil :font "DejaVu Sans Mono-11")

;; --- Miscellaneous ---

;; Autocomplete parentheses
(electric-pair-mode 1)

;; Show paren parentheses
(show-paren-mode 1)

;; Enables display of the current
;; highlighted line in all buffers
(global-hl-line-mode 1)

;; ---------------------------------
;; KEYBINDINGS AND FUNCTIONS
;; ---------------------------------

;; Move to the next buffer
(global-set-key (kbd "C-x C-?") 'next-buffer)

;; Move to the previous buffer
(global-set-key (kbd "C-x C-p") 'previous-buffer)

;; Switch between windows in reverse
(global-set-key (kbd "C-x C-o") 'other-window-backward)

;; Comment, uncomment, or add a comment at the end of the line
(global-set-key (kbd "C-x ,") 'comment-dwim)

;; Scroll N lines behind
(global-set-key (kbd "C-q") 'scroll-n-lines-behind)

;; Scroll N lines ahead
(global-set-key (kbd "C-z") 'scroll-n-lines-ahead)

;; Switch buffers using Ivy or Counsel
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

;; Magit status
;;(global-set-key (kbd "C-x g") 'magit-status)

;; Comment or uncomment a single line
(global-set-key (kbd "C-c C-f") 'comment-line)

;; Comment or uncomment a region
(global-set-key (kbd "C-c C-r") 'comment-or-uncomment-region)

;; Duplicate the current line
(global-set-key (kbd "C-c d") 'duplicate-line)

;; Change to Dracula theme
(global-set-key (kbd "C-c t d") 'my/load-dracula)

;; Change to Zenburn theme
(global-set-key (kbd "C-c t z") 'my/load-zenburn)

;; ---- FUNCTIONS ----

;; Switch between windows in reverse function definition
(defun other-window-backward(&optional n)
  "Select Nth previous window"
  (interactive)
  (other-window (- (prefix-numeric-value n))))

;; Scroll N lines behind function definition
(defun scroll-n-lines-behind(&optional n)
  "Scroll behind N lines"
  (interactive)
  (scroll-behind (prefix-numeric-value n)))

;; Scroll N lines ahead function definition
(defun scroll-n-lines-ahead(&optional n)
  "Scroll ahead N lines"
  (interactive)
  (scroll-ahead (prefix-numeric-value n)))

;; Duplicate line function definition
(defun duplicate-line()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; Load Dracula theme
(defun my/load-dracula()
  "Load Dracula theme"
  (interactive)
  (load-theme 'dracula t))

;; Load Zenburn theme
(defun my/load-zenburn()
  "Load Zenburn theme"
  (interactive)
  (load-theme 'zenburn t))

;; ---------------------------------
;;  PACKAGES
;; ---------------------------------

;; Moody
(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; Minions
(use-package minions
  :ensure t
  :config (minions-mode 1))

;; Magit
;;(use-package magit
  ;;:ensure t)

;; Yasnippets: shortcuts to complete
(use-package yasnippet
  :ensure t
  :init (yas-global-mode t))

;; Some snippets
(use-package yasnippet-snippets
  :ensure t)

;; Autocompletado con company 
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-clang)
  :bind ("C-<tab>" . company-yasnippet))

;; Mejora la navegación con ivy
(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Mejoramiento de counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c k" . counsel-rg)))

;; Golden ratio
(use-package golden-ratio
  :ensure t
  :config (golden-ratio-mode t))

;; All the icons
(use-package all-the-icons
  :ensure t)

;; File explorer
(use-package neotree
  :ensure t
  :bind ("C-x n" . neotree-toggle))

;; Aplica company-mode para código en C y C++
(use-package cc-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook 'company-mode)
  (add-hook 'c++-mode-hook 'company-mode))

;; ---------------------------------
;; LANGUAGE PROGRAMMING
;; ---------------------------------

;; Indentación de C y C++
(defun my-c-mode-hook()
  (setq c-basic-offset 4)
  (setq tab-width 4))
;; Establece la nueva indentacion a 4 espacios
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; Indentación de Python
(defun my-python-mode-hook()
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (setq python-shell-interpreter "python3"))
;; Establece la nueva indentacion a 4 espacios
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Configuración de Vala
(use-package vala-mode
  :ensure t)

;; Configuración de Golang
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (setq-default tab-width 4)
  (setq gofmt-command "gofmt")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq go-tab-width 4))

;; Rust-mode
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

;; C#-mode
(use-package csharp-mode
  :ensure t
  :config
  (setq c-basic-offset 4))

;; Ruby-mode
(use-package ruby-mode
  :interpreter "ruby"
  :config
  (setq ruby-indent-level 2))

;; Kotlin-mode
(use-package kotlin-mode
  :ensure t
  :config
  (setq kotlin-tab-width 4))

;; Haskell-mode
(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-indentation-layout-offset 4)
  (setq haskell-indentation-starter-offset 4))

;; Php-mode
(use-package php-mode
  :ensure t)

;; Elixir-mode
(use-package elixir-mode
  :ensure t)

;; Lua-mode
(use-package lua-mode
  :ensure t)

;; Cuda-mode
(use-package cuda-mode
  :ensure t)

;; Racket-mode
(use-package racket-mode
  :ensure t
  :config
  (setq racket-indent-offset 2))

;; Language Server Protocol (LSP)
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;;(use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode 1))

;; Configuración de Markdown
;; C-c C-c p (markdown-preview) Como se ve renderizado
;; M-RET nuevo encabezado
;; C-c C-s b: insertar texto en negritas
;; C-c C-s i: Insertar texto en itálicas
;; C-c C-c l: insertar un enlace
;; C-c C-c i: insertar una imagen
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.markdown\\'")
  :init
  (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t))

;; Configuraciín de Org-mode
;; Habilitar el soporte para imágenes en línea
(setq org-startup-with-inline-images t)
;; Permite el resaltado de sintaxis en bloques de código
(setq org-src-fontify-natively t)
;; Asegura que los bloques de código se alineen correctamente
(setq org-edit-src-content-indentation 0)


;; Configuración de CMake
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :init
  (setq cmake-tab-width 4)) ;; Indentación

;; Colorea qué paréntesis cierra con cuál
 ;;(use-package rainbow-delimiters
   ;;:hook (prog-mode . rainbow-delimiters-mode))

;; Ayuda para recomendar combinaciones de teclas
(use-package which-key
  :config (which-key-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cmake-mode rego-mode dracula-theme haskell-mode use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:foreground "#6c757d"))))
 '(line-number-current-line ((t (:background "#3a3c4e" :foreground "#ffffff" :weight bold))))
 '(show-paren-match ((t (:background "#44475a" :foreground "cyan" :weight bold))))
 '(show-paren-mismatch ((t (:background "#44475a" :foreground "orange" :weight bold)))))

;;; init.el ends here
