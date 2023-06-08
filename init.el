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

(global-hl-line-mode 1)

(fset 'yes-or-no-p 'y-or-n-p);Sets yes or no to y or no
(add-to-list 'image-types 'svg) ; Fixed inavlid type svg for macos

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

(use-package nerd-icons
 :custom
;; The Nerd Font you want to use in GUI
;; "Symbols Nerd Font Mono" is the default and is recommended
;; but you can use any other Nerd Font if you want
 (nerd-icons-font-family "Symbols Nerd Font Mono")
)

(use-package all-the-icons
:ensure t)

(use-package spaceline
:ensure t)
(use-package spaceline-config
:ensure spaceline
:config
(spaceline-spacemacs-theme)
(setq powerline-default-separator 'arrow))
(use-package spaceline-all-the-icons
:ensure t
:after spaceline
:config
(spaceline-all-the-icons-theme))

(use-package dashboard
:ensure t
:config
(setq dashboard-set-file-icons t)
(setq dashboard-display-icons-p t)
(setq dashboard-icon-type 'all-the-icons))
(dashboard-setup-startup-hook)

(setq org-html-validation-link nil)

(use-package org
  :ensure t
  :config
  (org-mode 1))

(setq org-log-into-drawer t);; Allows notes to be inserted into drawers

(use-package org-bullets
:ensure t
:config
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-hidden-keywords '(title))
(setq org-startup-indented t)
(setq org-startup-inline-images t)
(setq org-startup-folded t)

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

(setq org-log-done t)
(setq org-agenda-files '("~/Desktop/Org/Task.org"))
(global-set-key (kbd "C-c a") 'org-agenda)

(use-package evil
      :ensure t
      :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    :config
(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
      (evil-mode 1))

(use-package evil-collection
:after evil
:ensure t
:custom (evil-collection-setup-minibuffer t)
(setq evil-collection-most-list '(dired))
:init
(evil-collection-init))

(defun mp-elisp-mode-eval-buffer ()
  (interactive)
  (message "Evaluated buffer")
  (eval-buffer))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") #'mp-elisp-mode-eval-buffer)

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
 (global-corfu-mode -1)
(corfu-history-mode))

(use-package company
:ensure t
:init
(add-hook 'after-init-hook 'global-company-mode))

(use-package eglot
  :ensure t
  :hook
  ((rustic-mode-hook . eglot-ensure)))

(use-package rustic
:ensure t
:config
(setq lsp-rust-analyzer-completion-add-call-parenthesis nil))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))
