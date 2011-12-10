(setq auto-mode-alist
          (cons '("\\.hs$" .  haskell-mode) auto-mode-alist))

(add-to-list 'load-path (concat *home-elisp-path* "/haskell-mode-2.8.0/"))
(load "haskell-site-file")

(setq haskell-program-name "/usr/bin/ghci")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)