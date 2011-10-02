(setq auto-mode-alist
          (cons '("\\.ml[iyl]?$" .  caml-mode) auto-mode-alist))

;; you should probably comment the next line or replace ~remy by another path 
(add-to-list 'load-path (concat *home-elisp-path* "/ocaml.emacs/") )

(autoload 'caml-mode "ocaml" (interactive)
  "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")
