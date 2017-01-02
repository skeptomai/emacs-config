#!emacs --script

(require 'cl)
(require 'dash)

(defun list-installed-packages ()
  (lambda (s)
    (insert-string (format "package: %s\n" (symbol-name s))))
  (sort (-distinct (-non-nil package-activated-list)) 'string< ))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/packages/"))))


; activate all the packages (in particular autoloads)
(package-initialize)

; list the packages you want
(setq package-list (list-installed-packages))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (progn
    (insert-string (format "Checking package: %s\n" (symbol-name package)))
    (unless (package-installed-p package)
      (progn
        (insert-string (format "Installing package: %s\n" (symbol-name package)))
        (condition-case ex
            (package-install package)
          ('error (insert "failed this package...")))))))Checking package: async

;; (mapcar
;;  (lambda (s)
;;    (insert-string (format "package: %s\n" (symbol-name s))))
;;  (sort (-distinct (-non-nil package-activated-list)) 'string< ))
