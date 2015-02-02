(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake" ))
(add-to-list 'load-path (concat *home-elisp-path* "/go-mode.el/"))
(setenv "GOPATH" "/Users/cb/Projects/gopath")
(setenv "PATH" (concat (getenv "PATH") ":" "/usr/local/bin"))
(setq exec-path (append exec-path (list (expand-file-name "/Users/cb/Projects/gopath/bin"))))
(setq exec-path (append exec-path (list (expand-file-name "/usr/local/bin"))))

(require 'go-mode-load)
(require 'go-flymake)
(require 'go-flycheck)
