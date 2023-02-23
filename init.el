;; Atri's emacs config

(setq inhibit-startup-message t)

(tool-bar-mode -1)        ; Disable the toolbar
(menu-bar-mode -1)        ; Disable the menu bar
(tooltip-mode -1)         ; Disable the tooltips
(scroll-bar-mode -1)      ; Disable the scrollbar
(setq visible-bell t)     ; Enable visible bell

;; Make escape quit minibuffer
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(set-face-attribute 'default nil :font "JetBrains Mono" :height 140) ; Set font

(load-theme 'wombat) ; Set theme

;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https:// ") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package_install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Completion Engine
;; Ivy

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(doom-modeline ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
