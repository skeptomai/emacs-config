<<<<<<< HEAD:my-elisp/my-go-mode.el
(setenv "GOPATH" (expand-file-name (concat (getenv "HOME") "/Projects/gopath")))
(setenv "GOROOT" "/usr/local/go")
=======
>>>>>>> 681c9570fba187d0dfd22dba397ed83513c31436:my-elisp/my-go.el
(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name (concat (getenv "GOROOT") "/bin"))))
(add-to-list 'load-path (expand-file-name (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake")))
(setq exec-path (append exec-path (list (expand-file-name (concat (getenv "GOPATH") "/bin")))))
(setq exec-path (append exec-path (list (expand-file-name (concat (getenv "GOROOT") "/bin")))))

(add-hook 'before-save-hook #'gofmt-before-save)
(require 'go-mode-load)
(require 'go-autocomplete)
(require 'auto-complete-config)
(electric-pair-mode 1)
(yas-global-mode 1)
(ac-config-default)
