(autoload 'scheme-smart-complete "scheme-complete" nil t)

(eval-after-load 'scheme
  '(progn 
     (define-key scheme-mode-map "\e\t" 'scheme-smart-complete)
     (setq scheme-program-name "/usr/local/bin/racket")))
