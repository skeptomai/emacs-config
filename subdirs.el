(let* ((init-path (concat *home-elisp-path* "/my-elisp"))
       (my-elisps 
	'("haskell" "ruby" "scheme" "lisp" "haml" "baseline" "keys" "fns" "erc" "org" "bm" "tramp" "erlware" "cucumber" "textile-mode" "textile" "http-twiddle" "opscode-sign"))
       (default-directory *home-elisp-path*))
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path))
  (mapcar (lambda (x) (load-file (expand-file-name (concat "my-" x ".el") init-path))) my-elisps))

