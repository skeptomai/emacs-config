(setq auto-mode-alist
      (append '(("\\.feature$" . feature-mode)) auto-mode-alist))
(autoload 'feature-mode "cucumber-mode" "Cucumber feature mode" t nil)
