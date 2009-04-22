(require 'ruby-electric)
(autoload 'ruby-mode "ruby-mode" "ruby mode" t nil)
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby")
(autoload 'rubydb "rubydb3x" "Ruby Debugger" t)

(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Rakefile" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("\\.rake$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("\\.gemspec$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist
      (append '(("ruby" . ruby-mode)) interpreter-mode-alist))
(add-hook 'ruby-mode-hook
          '(lambda ()
            (progn
             (inf-ruby-keys)
             (define-key ruby-mode-map "\C-m" 'reindent-then-newline-and-indent)
             (ruby-electric-mode))))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

