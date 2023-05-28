(setq inhibit-startup-message t) ; Removes starting page

(tool-bar-mode -1);Determins if tool bar shows up
(scroll-bar-mode -1)
(menu-bar-mode 1)

(global-display-line-numbers-mode 1);Activates number lines
(setq display-line-numbers-type 'relative);Number lines are relative to the current line

(recentf-mode 1); loads recent files that you have edited

(setq history-length 20) ;Sets the amount of recent files tracked

(save-place-mode 1) ; Saves and restores last location of file

;; Moves custom variables to a seprate file and loads it
;; custom-file determines where custom variables are stored
;; locate-user-emacs-file Resolves path within the init.el directory
(setq custom-file (locate-user-emacs-file "custom-vars.el"));

;; Loads the custom file
;; Don't throw errors or put messages in minibuffer if errors occurs
(load custom-file 'noerror 'nomessage)

(setq use-dialog-box nil);Turns off graphical dialog box(less mouse clickey)

(global-auto-revert-mode 1);If file has changed, auto loads changes
(setq global-auto-revert-non-file-buffers t);;Same as above but for all buffers

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)

(use-package doom-themes
  :ensure t
  :config
   (setq doom-themes-enable-bold t   ; if nil, bold is universally disabled
         doom-themes-enable-italic t) ; if nil, italics is universally disabled
   (load-theme 'doom-acario-dark t))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

(use-package nerd-icons
 :custom
;; The Nerd Font you want to use in GUI
;; "Symbols Nerd Font Mono" is the default and is recommended
;; but you can use any other Nerd Font if you want
 (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package org
  :ensure t
  :config
  (org-mode 1))

(setq org-hidden-keywords '(title))
(setq org-startup-indented t)
(setq org-startup-inline-images t)

(use-package evil-org
:ensure t
:after (evil org)
:config
(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'evil-org-mode-hook
          (lambda ()
            (evil-org-set-key-theme '(navigation insert textobjects additional calendar))))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys))

;;select languages for bable
(org-babel-do-load-languages
'org-babel-load-languages
'((emacs-lisp . t)))

(setq org-confirm-babel-evaluate nil);;Confirmation to execute code block

(defun efs/org-babel-tangle-config()
(when(string-equal (buffer-file-name)
                   (expand-file-name "~/.emacs.d/WizzyMacs.org"))

(let ((org-confirm-babel-evaluate nil))
  (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package org-tempo)
(add-to-list 'org-structure-template-alist '("el". "src emacs-lisp"));;Autofill code blocks

(use-package evil
  :ensure t
  :config
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-mode 1))

(use-package vertico
:ensure t
:config
(vertico-mode 1))

(use-package marginalia
  :after vertico
  :ensure t
  :config
  (marginalia-mode 1))

(use-package savehist
:config
(savehist-mode))

(use-package which-key
  :ensure t 
  :config
(which-key-mode))

(use-package which-key
  :ensure t 
  :config
(which-key-mode))

(use-package flycheck
  :ensure t)

(use-package company
:ensure t)

(use-package corfu
 ;; Optional customizations
:custom
(corfu-cyclt)                ;; Enable cycling for `corfu-next/previous'

(corfu-auto t)                 ;; Enable auto completion
 ;; (corfu-separator ?\s)          ;; Orderless field separator
 ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
 ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
 ;; (corfu-preview-current nil)    ;; Disable current candidate preview
 ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
 ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
 ;; (corfu-scroll-margin 5)        ;; Use scroll margin

 ;; Enable Corfu only for certain modes.
 ;; :hook ((prog-mode . corfu-mode)
 ;;        (shell-mode . corfu-mode)
 ;;        (eshell-mode . corfu-mode))

 ;; Recommended: Enable Corfu globally.
 ;; This is recommended since Dabbrev can be used globally (M-/).
 ;; See also `corfu-exclude-modes'.
 :init
 (global-corfu-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :ensure t
  :config
  (setq lsp-keymap-prefix "C-c l")
  (lsp-enable-which-key-integration t))

(use-package eglot
  :ensure t
  :hook
  ((rustic-mode-hook . eglot-ensure)))

(use-package rustic
:ensure t
:config
(setq lsp-rust-analyzer-completion-add-call-parenthesis nil))
