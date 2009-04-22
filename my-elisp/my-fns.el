(defvar insert-time-format "%X"
  "Format for \\[insert-time] (c.f. 'format-time-string').")

(defvar insert-date-format "%x"
  "Format for \\[insert-date] (c.f. 'format-time-string').")


(defun insert-time()
  "Insert the current time according to insert-time-format"
  (interactive "*")
  (insert (format-time-string insert-time-format
			      (current-time))))

(defun insert-date()
  "Insert the current date according to insert-date-format"
  (interactive "*")
  (insert (format-time-string insert-date-format
			      (current-time))))

(defun point-to-top ()
  "Put point at top line of window"
  (interactive)
  (move-to-window-line 0))

(defun point-to-bottom ()
  "Put point at bottom line of window"
  (interactive)
  (move-to-window-line -1))

(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (if selective-display nil (or column 1))))

(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-one-line-ahead ()
  "Scroll one line ahead."
  (interactive)
  (scroll-ahead 1))

(defun scroll-one-line-behind ()
  "Scroll one line behind."
  (interactive)
  (scroll-behind 1))

(defun scroll-n-lines-ahead (&optional n)
  "Scroll  ahead n lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
  "Scroll  behind n lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))

(defun other-window-backward (&optional n)
  "Select nth previous window"
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

