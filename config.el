(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'elpaca-setup)  ;; The Elpaca Package Manager
(require 'buffer-move)   ;; Buffer-move for better window management
(require 'app-launchers) ;; Use emacs as a run launcher like dmenu (experimental)

;(use-package all-the-icons
;  :ensure t
;  :if (display-graphic-p))

;; I'm going to use nerd-icons instead...
;(use-package all-the-icons-dired
;  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Comment this if you want backup
;; and auto-save.
(setq make-backup-files nil)
(setq auto-save-default nil)

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "I'm an Evil Emacs User")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "/home/gael/.emacs.d/images/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package diminish)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
  (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
  (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file))

(use-package evil
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here 
  ;; for documentation purposes in case you need it.  
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))

(use-package evil-tutor)

(set-face-attribute 'default nil
  :font "JetBrains Mono"
  :height 110
  :weight 'medium)

(set-face-attribute 'variable-pitch nil
  :font "IBM Plex Sans"
  :height 120
  :weight 'bold)

(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 110
  :weight 'medium)

;; Makes commented text and keywords italics.
;; This is working in emacs client but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key.
  (general-create-definer dt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader.
    :global-prefix "M-SPC") ;; access leader in insert mode.

  (dt/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.emacs.d/config.org")) :wk "Edit emacs config")
    "f r" '(counsel-recentf :wk "Find recent files")
    "TAB TAB" '(comment-line :wk "Comment lines"))

  (dt/leader-keys
    "b" '(:ignore t :wk "Buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (dt/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired"))

  (dt/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")    
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e s" '(eshell :which-key "Eshell"))

   (dt/leader-keys
     "h" '(:ignore t :wk "Help")
     "h f" '(describe-function :wk "Describe function")
     "h t" '(load-theme :wk "Load theme")
     "h v" '(describe-variable :wk "Describe variable")
     "h r r" '((lambda () (interactive)
                (load-file "~/.emacs.d/init.el")
                (ignore (elpaca-process-queues)))
                :wk "Reload emacs config"))
  
   (dt/leader-keys
     "m" '(:ignore t :wk "Org")
     "m a" '(org-agenda :wk "Org agenda")
     "m e" '(org-export-dispatch :wk "Org export dispatch")
     "m i" '(org-toggle-item :wk "Org toggle item")
     "m t" '(org-todo :wk "Org todo")
     "m B" '(org-babel-tangle :wk "Org babel tangle")
     "m T" '(org-todo-list :wk "Org todo list"))

   (dt/leader-keys
     "m b" '(:ignore t :wk "Tables")
     "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

   (dt/leader-keys
     "m d" '(:ignore t :wk "Date/deadline")
     "m d t" '(org-time-stamp :wk "Org time stamp"))

   (dt/leader-keys
     "p" '(projectile-command-map :wk "Projectile"))

   (dt/leader-keys
     "t" '(:ignore t :wk "Toggle")
     "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
     "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
     "t t" '(visual-line-mode :wk "Toggle truncated lines"))

   (dt/leader-keys
     "w" '(:ignore t :wk "Windows")
     ;; Window splits.
     "w c" '(evil-window-delete :wk "Close window")
     "w n" '(evil-window-new :wk "New window")
     "w s" '(evil-window-split :wk "Horizontal split window")
     "w v" '(evil-window-vsplit :wk "Vertical split window")
     ;; Window motions.
     "w h" '(evil-window-left :wk "Window left")
     "w j" '(evil-window-down :wk "Window down")
     "w k" '(evil-window-up :wk "Window up")
     "w l" '(evil-window-right :wk "Window right")
     "w w" '(evil-window-next :wk "Goto next window")
     ;; Move Windows.
     "w H" '(buf-move-left :wk "Buffer move left")
     "w J" '(buf-move-down :wk "Buffer move down")
     "w K" '(buf-move-up :wk "Buffer move up")
     "w L" '(buf-move-right :wk "Buffer move right"))
)

(setq column-number-mode t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(global-hl-line-mode 1)

(display-time-mode 1)
(display-battery-mode 1)
(windmove-default-keybindings)

(use-package counsel
  :after ivy
  :diminish
  :config 
    (counsel-mode)
    (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

;; Using all-the-icons
;(use-package all-the-icons-ivy-rich
;  :ensure t
;  :init (all-the-icons-ivy-rich-mode 1))

;; Using nerd-icons
(use-package nerd-icons-ivy-rich
  :ensure t
  :init
  (nerd-icons-ivy-rich-mode 1)
  :custom
  (setq nerd-icons-ivy-rich-icon-size 5.0))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package csharp-mode)
(use-package dart-mode)
;;(use-package elixir-mode)
;;(use-package go-mode)
(use-package haskell-mode)
(use-package kotlin-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package php-mode)
(use-package racket-mode)
(use-package rust-mode)
(use-package vala-mode)

(setq-default indent-tabs-mode nil)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30           ;; sets modeline height
        doom-modeline-bar-width 5         ;; sets right bar width
        doom-modeline-icon t              ;; enables the icons
        doom-modeline-time-icon t         ;; enable the time icon
        doom-modeline-total-line-number t ;; displays the total lines of the file
        doom-modeline-persp-name t        ;; adds perspective name to modeline
        doom-modeline-persp-icon t))      ;; adds folder icon next to persp name

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))
(setq org-edit-src-content-indentation 0)

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-level-6 ((t (:inherit outline-5 :height 0.9))))
 '(org-level-7 ((t (:inherit outline-5 :height 0.8)))))

(require 'org-tempo)

(use-package pdf-tools
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("C-=" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)
                                                         (blink-cursor-mode -1)))

(use-package projectile
  :config
  (projectile-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(electric-pair-mode 1)
(show-paren-mode 1)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

  (use-package eshell-syntax-highlighting
    :after esh-mode
    :config
    (eshell-syntax-highlighting-global-mode +1))

  ;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
  ;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
  ;; eshell-aliases-file -- sets an aliases file for the eshell.

  (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
        eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
        eshell-allow-sudo t
        eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t
        eshell-destroy-buffer-when-process-dies t
        eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

;(use-package vterm
;  :config
;    (setq shell-file-name "/bin/zsh"
;          vterm-max-scrollback 5000))

;(use-package vterm-toggle
;  :after vterm
;  :config
;  (setq vterm-toggle-fullscreen-p nil)
;  (setq vterm-toggle-scope 'project)
;  (add-to-list 'display-buffer-alist
;               '((lambda (buffer-or-name _)
;                     (let ((buffer (get-buffer buffer-or-name)))
;                       (with-current-buffer buffer
;                         (or (equal major-mode 'vterm-mode)
;                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
;                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
;                  (reusable-frames . visible)
;                  (window-height . 0.4))))

(use-package sudo-edit
  :config
    (dt/leader-keys
      "f u" '(sudo-edit-find-file :wk "Sudo find file")
      "f U" '(sudo-edit :wk "Sudo edit file")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))
