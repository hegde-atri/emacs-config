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

;; (load-theme 'wombat) ; Set theme

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/packages/") t)
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
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

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

;; doom-themes
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "bf948e3f55a8cd1f420373410911d0a50be5a04a8886cabe8d8e471ad8fdba8e" default))
 '(package-selected-packages
   '(org-make-toc hydra evil-collection evil general doom-themes helpful counsel ivy-rich which-key rainbow-delimiters all-the-icons doom-modeline ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
