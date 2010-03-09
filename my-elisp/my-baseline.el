(require 'cl)

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Install color theme if it's here
(ignore-errors
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-clarity))

(defun unwriteroom ()
  (aquamacs-toggle-full-frame)
  (modify-frame-parameters (selected-frame) `((alpha . 100)))
  (set-window-margins nil 0 0)
  t)

(defun writeroom ()
  (modify-frame-parameters nil (list (cons 'fullscreen 'fullboth)))
  (set-window-margins nil 10 40)
  (set-frame-parameter (selected-frame) 'font "-apple-constantia-medium-i-normal--0-0-0-0-m-0-iso10646-1")
  (modify-frame-parameters (selected-frame) `((alpha . 65)))
  t)

(defun toggle-writeroom ()
  (interactive)
  (or 
   (and (frame-parameter nil 'fullscreen) (< (cdr (assoc 'alpha (frame-parameters))) 100) (unwriteroom))
   (writeroom)))

;; turn off prompting at exit
(if (fboundp 'aquamacs-save-buffers-kill-emacs)
    (defadvice aquamacs-save-buffers-kill-emacs (around no-query-kill-emacs activate)
      "Prevent annoying \"Active processes exist\" query when you quit Emacs."
      (flet ((process-list ())) ad-do-it))
    (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
      "Prevent annoying \"Active processes exist\" query when you quit Emacs."
      (flet ((process-list ())) ad-do-it)))

;; Check for {tool,menu,scroll}-bars and get rid of them
;; all this functionality is on the keyboard
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'show-paren-mode) (show-paren-mode t))
(when (fboundp 'tabbar-mode) (tabbar-mode nil))

;; Syntax colouring, show line and column numbers in status bar
(setq-default global-font-lock-mode t)
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default show-paren-mode t)
(setq-default desktop-save-mode t)
(setq-default desktop-save t)
(setq-default default-left-fringe-width 0
              default-right-fringe-width 0)

;; Enable recent file tracking & opening
(recentf-mode t)

;; Get rid of old buffers on schedule
(setq-default midnight-mode t)

;; Highlight the current line
(setq-default global-hl-line-mode t)

;; window switching made easy
(windmove-default-keybindings)

;; better window management
(winner-mode t)

;; better navigation
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Command completion in the mini-buffer
(icomplete-mode t)

;; better buffer management
(iswitchb-mode t)

;; Make lines wrap automagically in text mode
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; spaces instead of tabs by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; visual marking of regions 
(setq-default transient-mark-mode t)
(setq-default colon-double-space t)

;; I like ediff to split horizontally.  Default is vertically
(setq-default ediff-split-window-function 'split-window-horizontally)

;; Set sensible defaults for my environment
;; including browsing of hyperspec in emacs with w3m
;; I've only got this set on the Mac for now 

(defun add-default (p)
  (add-to-list 'default-frame-alist p))

(when 
    (or (eq window-system 'mac)
        (eq system-type 'darwin))
  (progn
    (setq w3m-command "/opt/local/bin/w3m")
    (setq browse-url-browser-function '(("hyperspec" . w3m-browse-url)
                                        ("weitz" . w3m-browse-url)
                                        ("." . browse-url-default-macosx-browser)))
;;                                        ("." . w3m-browse-url)))
    (set-terminal-coding-system 'utf-8-unix)
    (add-default (cons 'width 82))
    (add-default (cons 'height 48))))

(server-start) ;; startup emacsclient support

(ansi-term "bash" "localhost") ;; start a shell

(type-break-mode) ;; get me to stop working once in a while

(fset 'yes-or-no-p 'y-or-n-p) ;; answer 'y' instead of 'yes'

;; narrow-to-region is normally disabled.  I enable it
(put 'narrow-to-region 'disabled nil)
