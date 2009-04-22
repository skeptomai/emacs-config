(load-library "cucumber-mode.el")
(require 'cucumber)

(autoload 'feature-mode "feature-mode" "feature mode" t nil)
(setq auto-mode-alist
      (append '(("\\.feature$" . feature-mode)) auto-mode-alist))
