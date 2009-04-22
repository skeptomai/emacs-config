;; Lisp and slime
(require 'slime)
(slime-setup)

;; reading Lisp HTML docs in Emacs
(require 'w3m-load)
(setq browse-url-browser-function #'w3m-browse-url)

;; Mount the hyperspec from disk image if we are on a Mac
;; with the standard setup
(when
    (eq system-type 'darwin) 
  (condition-case nil
      (directory-files "/Volumes/HyperSpec-7-0")
    (error (call-process "/usr/bin/hdiutil" nil nil nil "mount" "/Users/cb/HyperSpec-7-0.dmg"))))

;; matching parens on forms
;; (setq-default blink-matching-paren t)

; better paren handling
;; (load-library "mic-paren.el")
;; (require 'mic-paren)
;; (paren-activate)
;; (setf paren-priority 'close)

;; (autoload 'paredit-mode "paredit"
;;   "Minor mode for pseudo-structurally editing Lisp code."
;;   t)

;; (define-key slime-mode-map (kbd "(") 'paredit-open-list)
;; (define-key slime-mode-map (kbd ")") 'paredit-close-list)

;; (define-key slime-mode-map (kbd "C-/") 'paredit-backward-slurp-sexp)
;; (define-key slime-mode-map (kbd "C-=") 'paredit-forward-slurp-sexp)

;; (define-key slime-mode-map (kbd "C-?") 'paredit-backward-barf-sexp)
;; (define-key slime-mode-map (kbd "C-+") 'paredit-forward-barf-sexp)

;; (define-key slime-mode-map (kbd "\"") 'paredit-doublequote)
;; (define-key slime-mode-map (kbd "\\\\") 'paredit-backslash)

;; (define-key slime-mode-map (kbd "RET") 'paredit-newline)
;; (define-key slime-mode-map (kbd "<return>") 'paredit-newline)

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;;                                  (paredit-mode +1)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;;(setq inferior-lisp-program "/Users/cb/Projects/lisptty/tty-lispworks -init /Users/cb/lisp/lispworks-init.lisp"
;;(setq inferior-lisp-program "/usr/local/bin/sbcl --userinit /Users/cb/lisp/sbcl-init.lisp"
(setq inferior-lisp-program "/Volumes/ccl/dx86cl64"
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      common-lisp-hyperspec-root "file:///Volumes/HyperSpec-7-0/HyperSpec-7-0/HyperSpec/"
      slime-startup-animation nil)

(defvar *lisps* 
  '((lispworks . ("/Users/cb/Projects/lisptty/tty-lispworks -init /Users/cb/lisp/lispworks-init.lisp"
                  "*slime-repl lispworks*"))
    (sbcl . ("/usr/local/bin/sbcl --userinit /Users/cb/lisp/sbcl-init.lisp" 
             "*slime-repl sbcl*"))))

(defun lisp-repl-name (lisp)
  (second (cdr (assoc lisp *lisps*))))

(defun lisp-command-line (lisp)
  (first (cdr (assoc lisp *lisps*))))

(defun lispworks ()
  "opens a lispworks slime session"
  (interactive)
  (setq inferior-lisp-program (lisp-command-line 'lispworks)
        lisp-indent-function 'common-lisp-indent-function
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        common-lisp-hyperspec-root "file:///Volumes/HyperSpec-7-0/HyperSpec-7-0/HyperSpec/"
        slime-startup-animation nil)
  (if (null (get-buffer (lisp-repl-name 'lispworks)))
      (slime)))

(defun sbcl ()
  "opens an sbcl slime session"
  (interactive)
  (setq inferior-lisp-program (lisp-command-line 'sbcl)
        lisp-indent-function 'common-lisp-indent-function
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        common-lisp-hyperspec-root "file:///Volumes/HyperSpec-7-0/HyperSpec-7-0/HyperSpec/"
        slime-startup-animation nil)
  (if (null (get-buffer (lisp-repl-name 'sbcl)))
      (slime)))
  
(defun cltl ()
  "Opens Common Lisp The Language in w3m"
  (interactive)
  (w3m-find-file "~/lisp/lisp web sites/www.supelec.fr/docs/cltl/cltl2.html"))

