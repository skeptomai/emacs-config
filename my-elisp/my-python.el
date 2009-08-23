;; ; Python Mode
(autoload 'python-mode "python" "python mode" t nil)
(setq auto-mode-alist
      (append '(("\.py$" . python-mode)) auto-mode-alist))

