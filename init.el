(setq inhibit-startup-message t); Removes starting page

(global-display-line-numbers-mode 1);Activates number lines
(setq display-line-numbers-type 'relative);Number lines are relative to the current line


(setq history-length 20) ;Sets the amount of recent files tracked

;; Moves custom variables to a seprate file and loads it
;; custom-file determines where custom variables are stored
;; locate-user-emacs-file Resolves path within the init.el directory
(setq custom-file (locate-user-emacs-file "custom-vars.el"));

;; Loads the custom file
;; Don't throw errors or put messages in minibuffer if errors occurs
(load custom-file 'noerror 'nomessage)

(setq use-dialog-box nil);Turns off graphical dialog box(less mouse clickey)

(setq global-auto-revert-non-file-buffers t);;Same as above but for all buffers

(fset 'yes-or-no-p 'y-or-n-p);Sets yes or no to y or no
;(add-to-list 'image-types 'svg) ; Fixed inavlid type svg for macos

(add-to-list 'default-frame-alist '(undecorated . t))

(global-auto-revert-mode 1);If file has changed, auto loads changes
(recentf-mode 1); loads recent files that you have edited
(scroll-bar-mode -1)
(menu-bar-mode 1)
(tool-bar-mode -1) ; if tool bar shows up
(save-place-mode 1) ; Saves and restores last location of file

(keymap-global-set "C-x C-b" 'buffer-menu)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

(set-register ?h '(file . "~/"))
(set-register ?e '(file . "~/.emacs.d/"))
(set-register ?i '(file . "~/.emacs.d/WizzyMacs.org"))
(set-register ?o '(file . "~/org/"))
(set-register ?s '(file . "~/org/College/"))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(add-to-list 'Info-default-directory-list "~/.emacs.d/info/")

(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package no-littering
:straight t)
(no-littering-theme-backups)

(use-package exec-path-from-shell
  :straight t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq tab-bar-show 1)

(keymap-global-set "C-x 5 w" 'set-frame-width)
(keymap-global-set "C-x 5 h" 'set-frame-height)

(define-key global-map (kbd "<f1>") #'toggle-frame-fullscreen)

(keymap-set global-map "C-c C-h" #'shell)

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t
	  evil-want-keybinding nil
	  evil-want-fine-undo t
	  evil-want-C-w-in-emacs-state t)
  :config
  (evil-set-initial-state 'calibredb-show-mode 'emacs)
  (evil-set-initial-state 'calibredb-search-mode'emacs)
  (evil-set-undo-system 'undo-redo)
  (keymap-set evil-insert-state-map "C-c" 'evil-normal-state)
  (keymap-unset evil-motion-state-map "<SPC>" t) 
  (evil-mode 1))

(use-package evil-collection
:after evil
:straight t
:config
(evil-collection-init))

(use-package org
  :defer
  :straight '(org
              :fork (:host nil
                     :repo "https://git.tecosaur.net/tec/org-mode.git"
                     :branch "dev"
                     :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
		(require 'lisp-mnt)
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents "lisp/org.el")
                        (lm-header "version")))
                     (git-version
                      (string-trim
                       (with-temp-buffer
                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                         (buffer-string)))))
                (insert
                 (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                 (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                 "(provide 'org-version)\n")))
              :pin nil) 
  :init (require 'ox-md)
  :bind (:map org-mode-map
	      ("C-c C-|" . org-table-insert-column)
	      ("C-c C-x i" . org-id-get-create))
  :hook (org-mode . flyspell-mode)
  :config
   (setq org-html-validation-link nil org-hide-emphasis-markers t
	   org-clock-sound "~/android.webm"
	   org-list-allow-alphabetical t
	   org-insert-heading-respect-content t)
   (keymap-global-set "C-c l" 'org-store-link)
   (keymap-global-set "C-c a" 'org-agenda)
   (keymap-global-set "C-c c" 'org-capture))

;; Increase preview width
 (plist-put org-latex-preview-appearance-options
            :page-width 0.8)

 ;; ;; Use dvisvgm to generate previews
 ;; ;; You don't need this, it's the default:
 ;; (setq org-latex-preview-process-default 'dvisvgm)
 
 ;; Turn on `org-latex-preview-mode', it's built into Org and much faster/more
 ;; featured than org-fragtog. (Remember to turn off/uninstall org-fragtog.)
 (add-hook 'org-mode-hook 'org-latex-preview-mode)

 ;; ;; Block C-n, C-p etc from opening up previews when using `org-latex-preview-mode'
 ;; (setq org-latex-preview-mode-ignored-commands
 ;;       '(next-line previous-line mwheel-scroll
 ;;         scroll-up-command scroll-down-command))

 ;; ;; Enable consistent equation numbering
 ;; (setq org-latex-preview-numbered t)

 ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
 ;; fragment and updates the preview in real-time as you edit it.
 ;; To preview only environments, set it to '(block edit-special) instead
 (setq org-latex-preview-mode-display-live t)

 ;; More immediate live-previews -- the default delay is 1 second
 (setq org-latex-preview-mode-update-delay 0.25)

(defun my/org-latex-preview-uncenter (ov)
    (overlay-put ov 'before-string nil))
  (defun my/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify)))
  (defun my/org-latex-preview-center (ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (when-let* ((elem (org-element-context))
                  ((or (eq (org-element-type elem) 'latex-environment)
                       (string-match-p "^\\\\\\[" (org-element-property :value elem))))
                  (img (overlay-get ov 'display))
                  (prop `(space :align-to (- center (0.55 . ,img))))
                  (justify (propertize " " 'display prop 'face 'default)))
        (overlay-put ov 'justify justify)
        (overlay-put ov 'before-string (overlay-get ov 'justify)))))
  (define-minor-mode org-latex-preview-center-mode
    "Center equations previewed with `org-latex-preview'."
    :global nil
    (if org-latex-preview-center-mode
        (progn
          (add-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter nil :local)
          (add-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter nil :local)
          (add-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center nil :local))
      (remove-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center)
      (remove-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter)))

(evil-define-key 'normal org-mode-map
  (kbd "SPC h") #'org-insert-heading
  (kbd "SPC a h") #'org-insert-heading-after-current
  (kbd "SPC s h") #'org-insert-subheading)

(setq org-goto-interface 'outline-path-completion)

(setq org-todo-keywords
      '((sequence
	 "TODO(t)" ;To be done
	 "HOLD(H)" ;In hiatus
	 "?(?)" ;To be Considered
	 "|"
	 "DONE(D)" ;Done 
	 "VOID(V)" ;Rendered Void
	)
	(sequence
	 "READ(r)" ;To be read
	 "|" 
	 )
	(sequence
	 "BUY(b)" ;To be bought
	 "|"
	 "BOUGHT(B)" 
	 )
	(sequence
	"FIX(f)" ;Not functioning as intended
	"|"
	"FIXED(F)"
	))
      org-tag-alist ;Controlled Vocabulary of tags
      '(("emacs" . ?e);Prima Facie(Self-evident)
	("info" . ?i) ;Information about Information
	("cmpt" . ?c) ;Computers & their sciences
	)
      org-fast-tag-selection-single-key 'nil) ;C-u once to show selection,twice to remove single-key-exit

(setq org-archive-location "archive/%s::") ;;Store items in archive files in seperate archive directory

(use-package org-tempo
:straight '(:type built-in)
:config
(setq org-structure-template-alist (append
				    '(("el" .  "src emacs-lisp"))
				    '(("cc" .  "src C"))
				    '(("L" . "src lisp")) org-structure-template-alist)))

(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-targets '((org-agenda-files   :maxlevel . 2)
			   (nil :maxlevel . 3 )))

;;select languages for bable
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (python . t)
     (shell . t)
     (scheme . t)
     (lisp . t)))

  
(setq org-babel-python-command "python3")
(setq org-confirm-babel-evaluate nil);;Confirmation to execute code block

(defun efs/org-babel-tangle-config()
(when(string-equal (buffer-file-name)
                   (expand-file-name "~/.emacs.d/WizzyMacs.org"))

(let ((org-confirm-babel-evaluate nil))
  (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(setq org-default-notes-file (expand-file-name "log.org" org-directory)
      org-capture-templates
	'(("c" "Capture" entry (file "") 
	   "* ?  %?\n\nCaptured on: %U")
	  ("e" "Emacs" entry (file "emacs.org")
	   "* ?  %?\n\nCaptured on: %U")
	  ;Made at 2:22pm Oct 2nd
	  ;Template to create entry for a class using org-goto to choose headline
	  ("s" "School" entry (file+function "school.org"
					     (lambda () (let ((org-goto-max-level  2)))
					       (org-goto)))
	   "* ?  %?\n\nCaptured on: %U")))

(setq org-hidden-keywords '(title)
      org-startup-indented t
      org-startup-with-inline-images t
      org-startup-folded t)

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode t))))

(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defadvice org-tree-to-indirect-buffer (before indirect-buffer-prefix-arg
					       activate compile)
  (interactive (list (cond ((numberp current-prefix-arg) (if (>= current-prefix-arg 0)
							     current-prefix-arg))
			   ((symbolp current-prefix-arg) current-prefix-arg)
			   ((listp current-prefix-arg) current-prefix-arg)))))

(setq org-log-state-notes-into-drawer "NOTES")

(use-package citar
  :custom
  (citar-bibliography '("~/org-roam/biblio.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :config
  (evil-define-key 'normal org-mode-map (kbd "SPC c i k") #'citar-insert-citation))

(use-package org-roam
  :straight  t
  :demand t
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  (org-roam-dailies-capture-templates '(("d" "default" entry "%?"
					 :target (file+head "%<%Y-%m-%d>.org"
							    "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates '(("d" "default" plain "%?" 
				 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE:${title}")
				 :unnarrowed t)
				("m" "Math" plain "%?"
				 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE:${title}\n#+FILETAGS: :math:")
				 :unnarrowed t)
				("c" "computer science" plain "%?"
				 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE:${title}\n#+FILETAGS: :cmpt:")
				 :unnarrowed t)
				("s" "Spanish" plain "%?"
				 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE:${title}\n#+FILETAGS: :span:lang:")
				 :unnarrowed t)
				("l" "Linguistics" plain "%?"
				 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE:${title}\n#+FILETAGS: :ling:")
				 :unnarrowed t)
				("e" "Emacs Related")
				("ee" "Emacs" plain "%?"
                                 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE:${title}\n#+FILETAGS: :emacs:")
				 :unnarrowed t)
				("el" "Elisp" plain "%?"
				 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE:${title}\n#+FILETAGS: :emacs:lisp:")
				 :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
	 ("C-c n t" . org-roam-dailies-capture-today)
	 ("C-c n C-t" . org-roam-dailies-goto-today)
	 ("C-c n p" . org-roam-dailies-goto-previous-note)
	 ("C-c n n" . org-roam-dailies-goto-next-note)) 
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template
	(concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
	org-roam-dailies-directory
	(expand-file-name "daily" org-roam-directory))
  (org-roam-db-autosync-mode)
    (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

    (setq org-roam-node-display-template
  	(concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))
  	org-roam-dailies-directory
  	(expand-file-name "daily" org-roam-directory))
    (org-roam-db-autosync-mode)

  (defun wiz/org-roam-node-from-cite (citeKey)
    (interactive (list (citar-select-ref)))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${title}.org"
                                       ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey citeKey) ;; Passes plist to info
                         :node (org-roam-node-create :title (citar-get-value 'title citeKey))
                         :props '(:finalize find-file ))))

(evil-global-set-key 'normal (kbd "C-c n r") #'wiz/org-roam-node-from-cite)

(use-package auctex
:straight t)

(use-package org-download
  :straight t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-download-screenshot-method "screencapture -i %s"
  org-download-image-dir "images"))

(evil-define-key 'normal org-mode-map (kbd "SPC SPC") #'org-download-screenshot)

(directory-files org-directory)

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t   ; if nil, bold is universally disabled
	doom-themes-enable-italic t)
  ) ; if nil, italics is universally disabled

(use-package cherry-blossom-theme
  :straight t)

(use-package tao-theme
  :straight t)

(use-package spacemacs-theme
  :straight t)

(load-theme 'spacemacs-dark)

(use-package nerd-icons
  :straight t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;;   (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package all-the-icons
:straight t)

(set-face-attribute 'default nil :height 150)
(set-frame-font "JetBrains Mono" nil t)
(global-hl-line-mode -1) 
(hl-line-mode -1)

(setq browse-url-mailto-function 'browse-url-generic)
(setq browse-url-generic-program "open")

(use-package vertico
  :straight t
  :config
  (vertico-mode 1))

(use-package marginalia
  :after vertico
  :straight t
  :config
  (marginalia-mode 1))

(use-package savehist
  :config
  (savehist-mode))

(use-package which-key
  :straight t 
  :config
  (which-key-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package flycheck
  :straight t)
(global-flycheck-mode)

(use-package company
  :config (setq company-idle-delay 0
		company-minimum-prefix-length 1
		company-tooltip-align-annotations t))
(add-hook 'after-init-hook 'global-company-mode)

(use-package geiser
  :straight t
  :custom
  (geiser-active-implementations '(racket)))

(use-package geiser-racket
  :straight t)

(use-package slime
  :straight t
  :config 
  (setq inferior-lisp-program "sbcl"))

(use-package eglot
  :straight (:type built-in)
  :hook ((python-mode . eglot-ensure)))

(use-package treemacs
  :straight t)
