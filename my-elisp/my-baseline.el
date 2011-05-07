(require 'cl)
(require 'find-lisp)

;; Generate inline-css so syntax-colored source code
;; is easier to cut-n-paste elsewhere
(setq htmlize-output-type "inline-css")

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Set directory for backup files
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat (getenv "HOME") "/emacs.d" "/backups")))))

;; Fix linum mode space between line numbers and text
(setq linum-format "%d  ")

;; Set paths on OS X the hard way, by reading /etc/paths and /etc/paths.d
(defun read-lines (fpath) 
  "Return a list of lines of a file at at FPATH." 
  (with-temp-buffer 
    (insert-file-contents fpath) 
    (split-string (buffer-string) "\n" t)))

(setq exec-path (mapcan
                 (lambda (x) (read-lines x))
                 (cons "/etc/paths"  (find-lisp-find-files "/etc/paths.d" "\.*"))))

(setenv "PATH"  (mapconcat 'identity exec-path ":" ))

;; I use aspell instead of ispell, installed with homebrew
(setq-default ispell-program-name "aspell")


;; Install color theme if it's here
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(defun color-theme-skeptomai ()
  "Color theme adapted from  Solofo Ramangalahy's theme, created 2000-10-18.
Black on wheat, includes faces for vm, ispell, gnus,
dired, display-time, cperl, font-lock, widget, x-symbol."
  (interactive)
  (color-theme-install
   '(color-theme-skeptomai
     ((background-color . "wheat")
      (background-mode . light)
      (background-toolbar-color . "#bfbfbfbfbfbf")
      (border-color . "#000000000000")
      (bottom-toolbar-shadow-color . "#737373737373")
      (cursor-color . "blue")
      (foreground-color . "firebrick")
      (top-toolbar-shadow-color . "#e6e6e6e6e6e6"))
     ((gnus-mouse-face . highlight)
      (goto-address-mail-face . info-xref)
      (ispell-highlight-face . highlight)
      (notes-bold-face . notes-bold-face)
      (setnu-line-number-face . bold)
      (tinyreplace-:face . highlight)
      (vm-highlight-url-face . bold-italic)
      (vm-highlighted-header-face . bold)
      (vm-mime-button-face . gui-button-face)
      (vm-summary-highlight-face . bold))
     (default ((t (nil))))
     (bbdb-company ((t (nil))))
     (blue ((t (:foreground "blue"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (border-glyph ((t (nil))))
     (cperl-here-face ((t (:foreground "green4"))))
     (cperl-pod-face ((t (:foreground "brown4"))))
     (cperl-pod-head-face ((t (:foreground "steelblue"))))
     (custom-button-face ((t (:bold t))))
     (custom-changed-face ((t (:background "blue" :foreground "white"))))
     (custom-documentation-face ((t (nil))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face ((t (:underline t :foreground "blue"))))
     (custom-group-tag-face-1 ((t (:underline t :foreground "red"))))
     (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
     (custom-modified-face ((t (:background "blue" :foreground "white"))))
     (custom-rogue-face ((t (:background "black" :foreground "pink"))))
     (custom-saved-face ((t (:underline t))))
     (custom-set-face ((t (:background "white" :foreground "blue"))))
     (custom-state-face ((t (:foreground "dark green"))))
     (custom-variable-button-face ((t (:underline t :bold t))))
     (custom-variable-tag-face ((t (:underline t :foreground "blue"))))
     (dired-face-boring ((t (:foreground "Gray65"))))
     (dired-face-directory ((t (:bold t))))
     (dired-face-executable ((t (:foreground "SeaGreen"))))
     (dired-face-flagged ((t (:background "LightSlateGray"))))
     (dired-face-marked ((t (:background "PaleVioletRed"))))
     (dired-face-permissions ((t (:background "grey75" :foreground "black"))))
     (dired-face-setuid ((t (:foreground "Red"))))
     (dired-face-socket ((t (:foreground "magenta"))))
     (dired-face-symlink ((t (:foreground "blue"))))
     (display-time-mail-balloon-enhance-face ((t (:background "orange"))))
     (display-time-mail-balloon-gnus-group-face ((t (:foreground "blue"))))
     (display-time-time-balloon-face ((t (:foreground "red"))))
     (ff-paths-non-existant-file-face ((t (:bold t :foreground "NavyBlue"))))
     (font-lock-comment-face ((t (:foreground "firebrick"))))
     (font-lock-doc-string-face ((t (:bold t :foreground "slateblue"))))
     (font-lock-emphasized-face ((t (:bold t :background "lightyellow2"))))
     (font-lock-function-name-face ((t (:foreground "forest green"))))
     (font-lock-keyword-face ((t (:bold t :foreground "violetred"))))
     (font-lock-other-emphasized-face ((t (:italic t :bold t :background "lightyellow2"))))
     (font-lock-other-type-face ((t (:bold t :foreground "orange3"))))
     (font-lock-preprocessor-face ((t (:bold t :foreground "mediumblue"))))
     (font-lock-reference-face ((t (:foreground "red3"))))
     (font-lock-string-face ((t (:foreground "blue4"))))
     (font-lock-type-face ((t (:bold t :foreground "steelblue"))))
     (font-lock-variable-name-face ((t (:foreground "magenta4"))))
     (font-lock-warning-face ((t (:bold t :background "yellow" :foreground "Red"))))
     (green ((t (:foreground "green"))))
     (gui-button-face ((t (:background "grey75" :foreground "black"))))
     (gui-element ((t (:background "lightgrey"))))
     (highlight ((t (:background "darkseagreen2"))))
     (info-node ((t (:underline t :bold t :foreground "mediumpurple"))))
     (info-xref ((t (:underline t :bold t :foreground "#0000ee"))))
     (isearch ((t (:background "paleturquoise"))))
     (italic ((t (:italic t))))
     (left-margin ((t (nil))))
     (list-mode-item-selected ((t (:background "gray68" :foreground "black"))))
     (modeline ((t (:bold t :background "Gray75" :foreground "Black"))))
     (modeline-buffer-id ((t (:bold t :background "Gray75" :foreground "blue4"))))
     (modeline-mousable ((t (:bold t :background "Gray75" :foreground "firebrick"))))
     (modeline-mousable-minor-mode ((t (:bold t :background "Gray75" :foreground "green4"))))
     (paren-blink-off ((t (:foreground "lightgrey"))))
     (paren-match ((t (:background "darkseagreen2"))))
     (paren-mismatch ((t (:background "DeepPink" :foreground "black"))))
     (pointer ((t (:foreground "blue"))))
     (primary-selection ((t (:background "gray65"))))
     (red ((t (:foreground "red"))))
     (region ((t (:background "black" :foreground "white"))))
     (right-margin ((t (nil))))
     (searchm-buffer ((t (:bold t :background "white" :foreground "red"))))
     (searchm-button ((t (:bold t :background "CadetBlue" :foreground "white"))))
     (searchm-field ((t (:background "grey89"))))
     (searchm-field-label ((t (:bold t))))
     (searchm-highlight ((t (:bold t :background "darkseagreen2" :foreground "black"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (template-message-face ((t (:bold t))))
     (text-cursor ((t (:background "blue" :foreground "lightgrey"))))
     (toolbar ((t (nil))))
     (underline ((t (:underline t))))
     (vertical-divider ((t (nil))))
     (widget-button-face ((t (:bold t))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (x-face ((t (:background "white" :foreground "black"))))
     (x-symbol-adobe-fontspecific-face ((t (nil))))
     (x-symbol-face ((t (nil))))
     (x-symbol-heading-face ((t (:underline t :bold t :foreground "green4"))))
     (x-symbol-info-face ((t (:foreground "green4"))))
     (x-symbol-invisible-face ((t (nil))))
     (x-symbol-revealed-face ((t (:background "pink"))))
     (yellow ((t (:foreground "yellow"))))
     (zmacs-region ((t (:background "yellow")))))))

(color-theme-arjen)

;; Add time to the info bar
(display-time-mode)

(defun unwriteroom ()
  (cond 
      ((fboundp 'aquamacs-toggle-full-frame)
       (aquamacs-toggle-full-frame))
      ((fboundp 'ns-toggle-fullscreen)
       (ns-toggle-fullscreen)))
  (modify-frame-parameters (selected-frame) `((alpha . 100)))
  (set-window-margins nil 0 0)
  t)

(defun writeroom ()
  (cond
      ((fboundp 'aquamacs-toggle-full-frame)
       (aquamacs-toggle-full-frame))
      ((fboundp 'ns-toggle-fullscreen)
       (ns-toggle-fullscreen)))
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
(setq-default desktop-save-mode t)
(setq-default desktop-save t)
(setq-default default-left-fringe-width 0
              default-right-fringe-width 0)
(setq-default text-scale-mode t)

;; Not sure where to put this
(add-hook 'c-mode-hook (lambda ()
                         (setq compilation-read-command nil)
                         (define-key c-mode-map "\C-c\C-k" 'compile) ))

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

;; filecache
(require 'filecache)

(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))

(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
	 (lambda ()
	   (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))

;; Change this to filter out your version control files
(add-to-list 'file-cache-filter-regexps "\\.svn-base$")
(add-to-list 'file-cache-filter-regexps "\\.git$")

(global-set-key (kbd "ESC ESC f") 'file-cache-ido-find-file)

;; Matching parentheses
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

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
    (set-default-font "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")
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

(switch-to-buffer "*scratch*") ;; don't want to be left in the term buffer

(type-break-mode) ;; get me to stop working once in a while

(fset 'yes-or-no-p 'y-or-n-p) ;; answer 'y' instead of 'yes'

;; narrow-to-region is normally disabled.  I enable it
(put 'narrow-to-region 'disabled nil)

;; I have gpg installed here, and epa/epg are available
(when (boundp 'epg-gpg-program) (setq epg-gpg-program "/usr/local/bin/gpg"))

