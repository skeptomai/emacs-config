;; Load the Opscode signing library
(load-library "opscode-sign.el")
;; Set your username and location of the request signing key
(setq opscode-userid "skeptomaijun21")
(setq opscode-userpem "/Users/cb/.chef/cb-user.pem")
;; Tell the edited version of http-twiddle to use Opscode's signing functions
(setq http-twiddle-signing-method 'opscode-sign-buffer-request)
(setq http-twiddle-show-request nil)
;; Make sure http-twiddle-mode gets loaded
(autoload 'http-twiddle-mode "http-twiddle" "HTTP Twiddle Mode" t nil)
(add-to-list 'auto-mode-alist '("\\.http-twiddle$" . http-twiddle-mode))
