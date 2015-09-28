(let* ((init-path (concat *home-elisp-path* "/my-elisp"))
       (my-elisps 
        '("baseline" "scala" "go-mode.el" "ruby" "haml" "keys" "fns" "erc" "org" "bm" "dash-at-point" "markdown"))
       (default-directory *home-elisp-path*))
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path))
  (mapcar (lambda (x) (load-file (expand-file-name (concat "my-" x ".el") init-path))) my-elisps))

;        '("haskell" "ruby" "scheme" "haml" "baseline" "keys" "fns" "erc" "org" "bm" "tramp" "erlware" "cucumber" "textile-mode" "textile" "http-twiddle" "opscode-sign" "dash-at-point" "go-mode.el" "gocode"))
