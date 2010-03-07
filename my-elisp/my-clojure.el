;; clojure-mode
(require 'clojure-mode)

;; swank-clojure
(add-to-list 'load-path (concat *home-elisp-path* "/my-elisp/swank-clojure/src/emacs"))

(setq swank-clojure-jar-path "~/Projects/clojure/clojure.jar"
      swank-clojure-extra-classpaths (list
				      (concat *home-elisp-path* "/my-elisp/swank-clojure/src/main/clojure")))

(require 'swank-clojure-autoload)

;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

(require 'slime)
(slime-setup)
