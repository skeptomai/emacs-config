(autoload 'http-twiddle-mode "http-twiddle" "HTTP Twiddle Mode" t nil)
(add-to-list 'auto-mode-alist '("\\.http-twiddle$" . http-twiddle-mode))
;; Sometimes I make calls to sites with self-signed certificates
(setq starttls-extra-arguments '("--insecure"))
