(setq inhibit-startup-message t) ; Removes starting page


(tool-bar-mode -1);Determins if tool bar shows up
(scroll-bar-mode -1)
(menu-bar-mode 1)

(global-display-line-numbers-mode 1);Activates number lines
(setq display-line-numbers-type 'relative);Number lines are relative to the current line

(load-theme 'modus-vivendi t) ; Loads theme

(recentf-mode 1); loads recent files that you have edited

(setq history-length 20) ;Sets the amount of recent files tracked
(savehist-mode 1) ; Activates file history

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

(add-to-list 'load-path "packages/evil.el")
(require 'evil)
(evil-mode 1)

