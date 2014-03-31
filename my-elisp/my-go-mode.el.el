(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/skeptomai/goflymake" ))
(add-to-list 'load-path (concat *home-elisp-path* "/go-mode.el/"))

(require 'go-mode-load)
(require 'go-flymake)
;; (require 'go-flycheck)



