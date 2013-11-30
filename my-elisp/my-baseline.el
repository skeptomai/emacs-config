(require 'cl)
(require 'find-lisp)

(setq org-export-latex-listings 'minted)
(setq org-export-latex-custom-lang-environments
      '((emacs-lisp "common-lispcode")))
(setq org-export-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
        ("linenos" "")))
(setq org-latex-to-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Don't bother me about the compile-command.  I trust you
(setq safe-local-variable-values '(compile-command) )
(setq enable-local-variables :safe)
;; Also, make it default to debug, because I'm lazy like that
(setq compile-command "make -k debug")

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
(load-theme 'deeper-blue t)

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
(setq-default fill-column 92)
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

(add-hook 'c++-mode-hook (lambda ()
                         (setq compilation-read-command nil)
                         (define-key c++-mode-map "\C-c\C-k" 'compile) ))

;; Enable recent file tracking & opening
(recentf-mode t)

;; Get rid of old buffers on schedule
(setq-default midnight-mode t)

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

(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

;; Blink the cursor so it's easier for my old eyes to find
(blink-cursor-mode)

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

;; I like raspberries.  No, really I like to have git recognize me
;; for things like gists
(let ((user (shell-command-to-string "git config --global --list | grep github.user | cut -d\"=\" -f2 | awk '{printf \"%s\", $0}'"))
      (token (shell-command-to-string "git config --global --list | grep github.token | cut -d\"=\" -f2 | awk '{printf \"%s\", $0}'")))
      (setq github-user user)
      (setq github-token token))

;; Set sensible defaults for my environment
;; including browsing of hyperspec in emacs with w3m
;; I've only got this set on the Mac for now 

(defun add-default (p)
  (add-to-list 'default-frame-alist p))

(when 
    (or (eq window-system 'mac)
        (eq system-type 'darwin))
  (progn
;;    (set-default-font "-apple-Bitstream_Vera_Sans_Mono-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")
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
