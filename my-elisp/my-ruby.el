(autoload 'ruby-mode "ruby-mode" "ruby mode" t)

(mapc
 (lambda (r) (add-to-list 'auto-mode-alist r))
     (list '("\\.rb$" . ruby-mode)
           '("\\.erb$" . ruby-mode)
           '("\\.rake$" . ruby-mode)
           '("Rakefile" . ruby-mode)
           '("Gemfile" . ruby-mode)
           '("\\.gemspec$" . ruby-mode)))

(add-to-list 'interpreter-mode-alist
             '("ruby" . ruby-mode))

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(autoload 'rubydb "rubydb3x" "Ruby Debugger" t)

(add-hook 'ruby-mode-hook
          '(lambda ()
            (progn
             (define-key ruby-mode-map "\C-m" 'reindent-then-newline-and-indent)
             (require 'ruby-electric)
             (ruby-electric-mode))))


(autoload 'yaml-mode "yaml-mode" "yaml mode" t)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

