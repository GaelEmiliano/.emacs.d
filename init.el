;; Desactiva la barra de herramientas
(menu-bar-mode -1)

;; Desactiva la barra de menú
(tool-bar-mode -1)

;; Muestra la barra de desplazamiento
(scroll-bar-mode t)

;; Añadimos las líneas a la izquierda
(global-display-line-numbers-mode 1)
;; Usa números de línea relativos
(setq display-line-numbers-type 'relative)

;; Cambia el tipo de letra a "Dejavu Sans Mono 11"
(set-face-attribute 'default nil :font "DejaVu Sans Mono-11")

;; Movernos al siguiente buffer
(global-set-key (kbd "C-x C-?") 'next-buffer)
;; Movernos al buffer anterior
(global-set-key (kbd "C-x C-p") 'previous-buffer)
;; Cambiar entre buffers
(global-set-key (kbd "C-x C-o") 'other-window-backward)
;; Agrega un comentario, lo quita, o lo añade al final
(global-set-key (kbd "C-x ,") 'comment-dwim)
;; Cambia al buffer seleccionado
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
;; Comenta una línea
(global-set-key (kbd "C-c C-c") 'comment-line)
;; Comenta una región
(global-set-key (kbd "C-c C-r") 'comment-or-uncomment-region)
;; Duplica una línea de código actual
(global-set-key (kbd "C-c d") 'duplicate-line)

;; Función que duplica línea de código actual
(defun duplicate-line()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; Configuración de repositorios de paquetes
;; Package management
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)
;; Actualiza la lista de paquetes si no está ya disponible
(unless package-archive-contents
  (package-refresh-contents))
;; Instala si no está ya instalado
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Indentacion de C y C++
(defun my-c-mode-hook()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
;; Establece la nueva indentacion a 4 espacios
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; Indentacion de Python
(defun my-python-mode-hook()
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
;; Establece la nueva indentacion a 4 espacios
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Configuración de Golang
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (setq tab-width 4) ;; Ancho de tabulación a 4 espacios
  (setq gofmt-command "gofmt") ;; Usar gofmt para formatear el código
  (add-hook 'before-save-hook 'gofmt-before-save) ;; Formatear
  (setq indent-tabs-mode nil) ;, Usar espacios en lugar de tabs
  (setq go-tab-width 4)) ;; Especificamente para Go

;; Configuración de Rust
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

;; Configuración de Ruby
(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :interpreter "ruby")

;; Configuración de Kotlin
(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'")

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Autocompletado con company
(use-package company
  :ensure t
  :init (global-company-mode 1))

;;(use-package flycheck
;;  :ensure t
;;  :init (global-flycheck-mode 1))

;;(use-package yasnippet
;;  :ensure t
;;  :init (yas-global-mode 1))

;; Haskell-mode
(use-package haskell-mode)

;; PHP-mode
(use-package php-mode)

;; Racket-mode
(use-package racket-mode)

;; Un tema oscuro (muy famoso)
(use-package dracula-theme
  :ensure t
  :init
  (setq dracula-theme t)
  (load-theme 'dracula t))

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

;; Autocompletar paréntesis
(electric-pair-mode 1)

;; Habilita cambiar de ventana usando Shift + flechas
(windmove-default-keybindings)

;; Guarda el estado de las ventanas
(desktop-save-mode 1)

;; Da el historial de comandos
(recentf-mode 1)
;; Guarda los últimos 25 archivos abiertos
(setq recentf-max-menu-items 25)

;; No crear archivos de respaldo
(setq make-backup-files nil)

;; No crear archivos de autoguardado
(setq auto-save-default nil)

;; Utiliza una señal visual para la campana
;; en lugar de un sonido
(setq visible-bell t)

;; Usa espacios en lugar de tabs para la indentación
(setq-default indent-tabs-mode nil)

;; Desactiva el registro de mensajes en el buffer *Messages*
(setq-default message-log-max nil)
;; Cierra o mata el buffer *Messages* si está abierto
(kill-buffer "*Messages*")

;; Habilita la visualización de la línea actual resaltada en
;; todos los buffers
(global-hl-line-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages '(rego-mode dracula-theme haskell-mode use-package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#483d8b")))))
(put 'set-goal-column 'disabled nil)
