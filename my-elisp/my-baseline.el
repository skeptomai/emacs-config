;; Remove splash screen
(setq inhibit-splash-screen t)

;; Check for {tool,menu,scroll}-bars and get rid of them
;; all this functionality is on the keyboard
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'show-paren-mode) (show-paren-mode t))
(when (fboundp 'tabbar-mode) (tabbar-mode t))

;; Syntax colouring, show line and column numbers in status bar
(setq-default global-font-lock-mode t)
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default desktop-save-mode t)
(setq-default desktop-save t)

;; Get rid of old buffers on schedule
(setq-default midnight-mode t)

;; Highlight the current line
(setq-default global-hl-line-mode t)

;; window switching made easy
(windmove-default-keybindings)

;; better window management
(winner-mode t)

;; line numbering when I want it
(require 'linum)

;; better navigation
(ido-mode t)
(setq ido-enable-flex-matching t)

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

;; Set elisp library load paths
(defvar *paths* 
  '( "/" "/slime/" "/w3m" "/nxml-mode-20041004" "/distel/"
    "/muse/lisp" "/git" "/remember-1.9" "/cucumber" "/misc"))

(mapcar (lambda (p) (add-to-list 'load-path (concat *home-elisp-path* p))) *paths*)


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
    (add-default (cons 'height 48)))
  (server-start))


;; startup sequence for when we've got a window system
;; on linux and want to start gnuserv
(when (and 
       (not (eq window-system nil))
       (eq system-type 'gnu/linux))
  (progn
    ;; start gnuserv, so apps can talk to us (e.g. p4, browsers)
    (ignore-errors
      (require 'gnuserv)
      (setq gnuserv-frame (selected-frame))
      (gnuserv-start))))

(type-break-mode) ;; get me to stop working once in a while

(fset 'yes-or-no-p 'y-or-n-p) ;; answer 'y' instead of 'yes'

;; miscellaneous junk
(put 'narrow-to-region 'disabled nil)