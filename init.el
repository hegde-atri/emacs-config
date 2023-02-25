;; Atri's emacs config

(setq inhibit-startup-message t)

(tool-bar-mode -1)        ; Disable the toolbar
(menu-bar-mode -1)        ; Disable the menu bar
(tooltip-mode -1)         ; Disable the tooltips
(scroll-bar-mode -1)      ; Disable the scrollbar
(setq visible-bell nil)     ; Enable visible bell

(column-number-mode)      ; Enable column number
(global-display-line-numbers-mode t)   ; Enable line numbers
(setq display-line-numbers 'relative)

;; Disable line buffers in some modes
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make escape quit minibuffer
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 125) ; Set font
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 125)
(set-face-attribute 'variable-pitch nil :font "Overpass" :height 140 :weight 'regular)

;; (load-theme 'wombat) ; Set theme

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Completion Engine
;; Ivy
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

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Doom modeline with all the icons
(use-package all-the-icons
  :ensure t)
;; doom-themes
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 40)
	   (doom-modeline-battery t)))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Which keys
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Helpful package
(use-package helpful
  :custom
  (counsel-describe-function-fuction #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-create-definer ha/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (ha/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "Choose theme")))

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

;; Hydra
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(ha/leader-keys
  "ts" '(hydra-text-scale/body :which-key "Scale text"))

(use-package org-make-toc)

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

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; TODO, setup
(use-package forge)

(defun ha/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

;; Replace list hyphen with dot
(defun ha/org-font-setup ()
 (font-lock-add-keywords 'org-mode
                         '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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

(defun ha/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ha/org-mode-visual-fill))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

  ;; Automatically tangle emacs.org whenever it is saved.
  (defun ha/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                       (expand-file-name "~/.emacs.d/emacs.org"))
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))
  
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ha/org-babel-tangle-config)))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" default))
 '(package-selected-packages
   '(visual-fill-column org-bullets forge magit counsel-projectile projectile org-make-toc hydra evil-collection evil general doom-themes helpful counsel ivy-rich which-key rainbow-delimiters all-the-icons doom-modeline ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
