(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake" ))
(add-to-list 'load-path (concat *home-elisp-path* "/go-mode.el/"))
(setenv "GOPATH" "/Users/cb/Projects/gopath")
(setq exec-path (append exec-path (list (expand-file-name "/Users/cb/Projects/gopath/bin"))))

(require 'go-mode-load)
(require 'go-flymake)
(require 'go-flycheck)
