;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contentts)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(tooltip-mode -1)        ; Disable tooltips
(menu-bar-mode -1)       ; Disable the menu bar
(tool-bar-mode -1)       ; Disable the tool bar
(scroll-bar-mode -1)     ; Disable the scrollbar
(setq visible-bell nil)  ; Visible bell disabled

(column-number-mode)                       ; Enable column number
(global-display-line-numbers-mode t)       ; Enable line numbers
(setq display-line-numbers 'relative)      ; Make line numbers relative

(dolist (mode '(term-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 125)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 125)
(set-face-attribute 'variable-pitch nil :font "Overpass" :height 140 :weight 'regular)

(use-package all-the-icons
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (
    (doom-modeline-height 40)
    (doom-modeline-battery t)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package swiper)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1))
(use-package counsel
  :bind(("M-x" . counsel-M-x)
        ("C-x b" . counsel-ibuffer)
        ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-keybinding nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

(use-package general
  :config
  (general-create-definer ha/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(ha/leader-keys
 "t"  '(:ignore t :which-key "toggles")
 "tt" '(counsel-load-theme :which-key "Choose theme"))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(ha/leader-keys
  "ts" '(hydra-text-scale/body :which-key "Scale text"))

(defun ha/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

;; Replace list hyphen with dot.
(defun ha/org-font-setup ()
  (font-lock-add-keywords 'org-mode
                         '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Change font size of headings. 
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.4)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.25)
                  (org-level-5 . 1.2)
                  (org-level-6 . 1.15)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.05)))
    (set-face-attribute (car face) nil :font "Overpass" :weight 'bold :height (cdr face)))

;; Fonts in org
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . ha/org-mode-setup)
  :config
  (setq org-ellipsis " ▼ "
        org-hide-emphasis-markers t)
  (ha/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (setq org-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")))

(use-package org-make-toc)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("python" . "src python"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))
(add-to-list 'org-structure-template-alist '("cf" . "src conf"))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

;; Automatically tangle emacs.org whenever it is saved.
(defun ha/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                     (expand-file-name "~/.emacs.d/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ha/org-babel-tangle-config)))

(defun ha/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ha/org-mode-visual-fill))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; TODO, setup
(use-package forge)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/repos")
    (setq projectile-project-search-path'("~/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(defun ha/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . ha/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; Enable debugger
(use-package dap-mode
  :after lsp-mode)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :defer t)

(use-package go-mode
  :hook (go-mode . lsp-deferred))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-nerd-commenter yaml-mode which-key web-mode visual-fill-column use-package typescript-mode rust-mode rainbow-delimiters org-make-toc org-bullets lsp-ui lsp-ivy ivy-rich helpful go-mode general forge evil-collection doom-themes doom-modeline dap-mode counsel-projectile company-box cargo all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
