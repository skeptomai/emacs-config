;; erlang-exports.el -- on-the-fly exports generator

;; Author:  Christopher Brown (skeptomai) <skeptomai@opscode.com>
;; Maintainer: Christopher Brown (skeptomai) <skeptomai@opscode.com>

;;; Commentary:
;;
;; Runs an erlang source file through the Erlang compiler 'erlc'
;; with the -S option, dumping the abstract syntax tree
;; We then read that tree, looking for functions and their arity


;;; Code:

(eval-when-compile (require 'cl))

(defvar *erlang-exports-files* (list)
  "association list of source files to their AST files")
(defvar *erlang-compile-command* "erlc")
(defvar *erlang-ast-function-expression* "^\{function,\s*\\([a-zAb-Z0-9_]*\\),\s\\([0-9]*\\),\s[0-9]*")

(defun erlang-exports-build-exports (new-buffer-name)
  "gathers exports from the Erlang AST"
  (let ((exports-list (list))
        (lines (count-lines (point-min) (point-max))))
    (goto-line 0)
    (beginning-of-line)
    (dotimes (line-num lines)
      (let* ((line-limit (line-end-position))
             (expression-match (re-search-forward *erlang-ast-function-expression* line-limit t)))
        (when expression-match
          (let ((matched-string 
                 (concat (buffer-substring-no-properties (match-beginning 1) (match-end 1))
                         "/"
                         (buffer-substring-no-properties (match-beginning 2) (match-end 2))))) ;;(line-beginning-position) (point))))
            (push matched-string exports-list))))
      (forward-line 1)
      (beginning-of-line))
    (goto-line 0)
    (beginning-of-line)
    (set-buffer (get-buffer-create new-buffer-name))
    (let* ((ordered-export-list (reverse exports-list))
           (oel-length (length ordered-export-list)))
      (when (> oel-length 0)
        (insert "-export([\n")
        (dolist (export ordered-export-list)
          (unless (string-match "module_info" export)
            (insert (concat "\t" export (if (> (decf oel-length) 2) ",\n" "")))))
        (insert "\n]).\n")))))


;; get the buffer-file-name for the erlang file
;; get the directory
;; get the file name bare and extension
;; change to that directory
;; call erlc -S filename
;; load and parse "bare file name".S

(defun erlang-exports ()
  (interactive)
  (multiple-value-bind (source-file-directory source-bare-file)
      (source-directory-and-file (buffer-file-name))
    (let* ((ast-file-name (concat source-bare-file ".S"))
           (new-buffer-name (concat "*erlang-exports-" ast-file-name "*")))
      (save-current-buffer
        (setq erlang-exports-process 
              (apply 'start-process
                     "erlang-exports-process" 
                     (get-buffer-create "*erlang-exports*") 
                     *erlang-compile-command* 
                     (build-command-line (buffer-file-name) ast-file-name) ))
        (push (cons buffer-file-name ast-file-name) *erlang-exports-files*)
        (set-process-sentinel erlang-exports-process 'erlang-exports-process-sentinel)))))

(defun build-command-line (source-file-name out-file-name)
  `("-S" ,source-file-name "-o " ,out-file-name))

(defun erlang-exports-process-sentinel (process event)
  (let* ((current-file-name (buffer-file-name))
         (ast-file-name (cdr (assoc current-file-name *erlang-exports-files*)))
         (new-buffer-name (concat "*erlang-exports-" ast-file-name "*"))
         (new-buffer-point nil))
    (with-temp-buffer new-buffer-name
      (insert-file-contents ast-file-name)
      (erlang-exports-build-exports new-buffer-name)
      (setq new-buffer-point (point)))
    (insert-buffer-substring new-buffer-name (point-min) new-buffer-point)
    (kill-buffer new-buffer-name)))

(defun source-directory-and-file (source-file-name)
  "separate the file name from the directory, call with buffer-file-name"
  (let* ((source-file-directory (file-name-directory source-file-name)))
    (string-match source-file-directory (file-name-sans-extension source-file-name))
    (values source-file-directory (substring (file-name-sans-extension source-file-name) (match-end 0)))))


;;========================================================================

(defun erlang-exports-start-process (cmd args dir)
  "Start export gathering process."
  (let* ((process nil))
    (condition-case err
	(progn
	  (when dir
	    (let ((default-directory dir))
	      (flymake-log 3 "starting process on dir %s" default-directory)))
	  (setq process (apply 'start-process "erlang-exports-proc" (current-buffer) cmd args))
	  (set-process-sentinel process 'erlang-exports-process-sentinel)
	  (set-process-filter process 'erlang-exports-process-filter)
          (push process erlang-exports-processes)

          (setq erlang-exports-is-running t)
          (setq erlang-exports-last-change-time nil)
          (setq erlang-exports-check-start-time (erlang-exports-float-time))

	  (flymake-report-status nil "*")
	  (flymake-log 2 "started process %d, command=%s, dir=%s"
		       (process-id process) (process-command process)
                       default-directory)
	  process)
      (error
       (let* ((err-str (format "Failed to launch syntax check process '%s' with args %s: %s"
			       cmd args (error-message-string err)))
	      (source-file-name buffer-file-name)
	      (cleanup-f        (flymake-get-cleanup-function source-file-name)))
	 (flymake-log 0 err-str)
	 (funcall cleanup-f)
	 (flymake-report-fatal-status "PROCERR" err-str))))))


