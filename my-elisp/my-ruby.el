
(autoload 'ruby-mode "ruby-mode" "ruby mode" t)

(add-to-list 'auto-mode-alist
      '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist
      '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist
      '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist
      '("\\.gemspec$" . ruby-mode))
(add-to-list 'interpreter-mode-alist
      '("ruby" . ruby-mode))

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby")
(autoload 'rubydb "rubydb3x" "Ruby Debugger" t)

(add-hook 'ruby-mode-hook
          '(lambda ()
            (progn
             (inf-ruby-keys)
             (define-key ruby-mode-map "\C-m" 'reindent-then-newline-and-indent)
             (require 'ruby-electric)
             (ruby-electric-mode))))


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

