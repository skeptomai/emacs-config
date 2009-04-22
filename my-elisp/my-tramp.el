;; adds ssh username & host prefix for remote slime
;; editing [cb]
(require 'tramp)
(defvar *tramp-remote-host* "10.0.2.21")
(defvar *tramp-remote-user* "cb")

(defmacro set-tramp-host-and-user ()
  "Sets the user and host for remote (ssh) tramp editing"
  (setf slime-filename-translations nil)
  `(push (list (concat "^" ,*tramp-remote-host* "$")
              (lambda (emacs-filename)
                (substring emacs-filename (length (concat "/ssh:" ,*tramp-remote-user* "@" ,*tramp-remote-host* ":")) ))
              (lambda (lisp-filename)
                (concat "/ssh:" ,*tramp-remote-user* "@" ,*tramp-remote-host* ":" lisp-filename)))
        slime-filename-translations))
;; (set-tramp-host-and-user)

