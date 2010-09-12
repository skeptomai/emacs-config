; textile.el
; $Id: textile.el,v 1.146 2008-08-11 19:41:03 csebold Exp $

; by Charles Sebold <csebold@gmail.com>

;;; Copyright: (C) 2004, 2008 Charles Sebold
;; 
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with GNU Emacs; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Latest version should be available at:
;;    <URL:http://zancanda.staticcling.org/emacs/textile/>

; Note - in comments, DA = Dean Allen (original author of PHP Textile),
; BC = Brad Choate (implemented Textile v2 in Perl for Movable Type)

; To use textile.el, put this file into your load path, and put the
; following into your .emacs file:
;
; (require 'textile)
;
; At this time Textile markup is only documented at
; <http://www.textism.com/tools/textile/> and
; <http://bradchoate.com/mt/docs/mtmanual_textile2.html>, so to
; understand how to write Textile code, for now you should check those
; web sites.  In the future there will be a Texinfo manual detailing the
; format that comes with textile.el.
;
; In a buffer with Textile markup, you can run:
;
;   M-x textile-buffer RET
;
; and have the entire buffer processed by Textile, which (by default)
; will create a new buffer with valid XHTML markup in it.  If you want
; the returned XHTML in the same buffer as the old code, you can change
; textile-output-to-new-buffer to nil (it defaults to t).  You can also
; call textile-region in similar fashion.  If you want to use textile.el
; in your own Emacs Lisp programs, you can pass a string to
; textile-string and it will return XHTML in another string.
;
; Standard Textile behavior is to treat newlines in the middle of a
; block (like a paragraph) as intentional newlines, and it replaces them
; with <br /> when it sees them.  To change this behavior so that you
; can edit Textile text with auto-fill-mode on, you can either load
; longlines.el (found elsewhere) or you can change
; textile-br-all-newlines to nil (it defaults to t).  It is hard to
; guarantee that textile.el will always do the right thing if you change
; textile-br-all-newlines, and you may find it difficult in some cases
; to enter a manual linebreak unless you escape it with ==; the best
; thing to do is either edit without auto-fill-mode or use longlines,
; but I will try to keep textile-br-all-newlines working as expected
; when possible (because I like auto-fill-mode).
;
; Do NOT send bug reports on textile.el to Dean Allen or Brad Choate;
; they had nothing to do with the Emacs implementation.  Send bug
; reports to csebold@gmail.com, preferably along with sample text and
; some description of what you expected to see.

; Bugs from BC's test cases which I wasn't sure how to annotate yet:

;; FIXME: testcases.txt, line 635, not sure I agree with title handling
;; FIXME: testcases.txt, line 645, doubled the "caps" span, oops
;; FIXME: testcases.txt, line 665, BC skips groups of asterisks greater
;;                                 than two
;; FIXME: testcases.txt, stopped reading the test results after line 665

(defvar textile-version "Textile.el v0.99.1"
  "Version number for textile.el.")

(defvar textile-block-tag-regexp-start "^\\("
  "All textile tag regexps start with this.")
(defvar textile-block-any-tag-regexp "[a-z0-9]+"
  "Regexp corresponding to the basic xx. tag.
This doesn't count lists and escaped parts.")
; This next one might be able to be replaced by the new textile-attributes
; code, which just reads from the end of the tag to the end of the attrib
; information
(defvar textile-block-tag-regexp-end
  "\\)\\(.*?\\)\\(\\.\\{1,2\\}\\)\\(?: \\|\n\\)"
  "This is how all block tags are supposed to end.")

(defvar textile-block-tag-regexp
  (concat textile-block-tag-regexp-start
          textile-block-any-tag-regexp
          textile-block-tag-regexp-end)
  "Corresponds to standard Textile block tags.
This puts the previous pieces together, making it easier for
us to construct alternate block tag regexps.")

(defvar textile-any-block-tag-regexp
  "^\\(?:[a-z0-9]+\\.\\|[^ ]+?[*#]\\|==\\||\\)"
  "Corresponds to any Textile block tag.
All block tags must match this in some way; useful for
determining where an extended block ends.")

(defun textile-this-block-tag-regexp (tag)
  "Create a block tag regexp out of TAG."
  (concat textile-block-tag-regexp-start
          tag
          textile-block-tag-regexp-end))

(defvar textile-list-tag-regexp
  ; FIXME - adapt code for table rows to this regexp for lists?
  "^\\(([^ ]+?)\\|\\)\\([*#]+\\)[^ ]* "
  "All list block tags must match this.")

(defvar textile-inline-tag-regexp
  (concat
   "\\(?:^\\|\\W\\)\\("
   "[_]\\{1,2\\}\\|[+]\\{1,2\\}\\|[-]\\{1,2\\}\\|[~^%@]"
   "\\|\\?\\?\\|[*]\\{1,2\\}"
   "\\)\\([^\000]+?\\)\\(\\1\\)\\(?:$\\|\\W\\)")
  "Should match any inline tag.")

(defvar textile-inline-tag-list
  '("*" "strong" "_" "em" "**" "b" "__" "i" "++" "big" "--" "small"
    "-" "del" "+" "ins" "^" "sup" "~" "sub" "%" "span" "@" "code" "??" "cite")
  "Link textile to HTML tags for inline formatting.")

(defvar textile-alias-list-defaults nil
  "Standard link aliases.
For each string to match should be either a string which is the URL, or
a list whose car is the title and cadr is the URL.")

(defvar textile-macros-list-defaults
  '("->" "&#8594;" "(C)" "&#169;" "(R)" "&#174;" "(TM)" "&#8482;"
    "x\\([0-9]\\)" "&#215;\\1" ; 3x3
    "<-" "&#8592;")
  "Code to be automatically converted to HTML entities or other things.")

(defvar textile-smart-quotes-list
  '("\\(^\\| \\)--\\( \\|$\\)" "\\1&#8212;\\2" ; em-dash
    "\\(^\\| \\)-\\( \\|$\\)" "\\1&#8211;\\2" ; en-dash
    "\\.\\( ?\\)\\.\\1\\." "&#8230;" ; ellipsis
    "\\(\\w\\)'\\(\\w\\)" "\\1&#8217;\\2" ; word-apostrophe-letter
    "\\([^ ]\\)'s" "\\1&#8217;s" ; special case apostrophe-s
    "'\\([0-9]\\{2\\}s\\)" "&#8217;\\1" ; decades like the '80s
    " \"" " &#8220;" ; any double-quote preceded by a space
    " '" " &#8216;" ; any single-quote preceded by a space
    "\"\\( \\|$\\)" "&#8221;\\1" ; any double-quote followed by space
    "'\\( \\|$\\)" "&#8217;\\1" ; any single-quote followed by space
    "\\([0-9]\\)'\\([0-9]\\|x[0-9]\\)" "\\1&#8217;\\2" ; 5'11 works
    "\\([0-9]\\)\"\\([0-9]\\|x[0-9]\\)" "\\1&#8221;\\2" ; 5'11" works
    "\\`\"\\|\"\\b" "&#8220;" "\\b\"\\|\"\n\\|\"\\'" "&#8221;"
    "\\`'\\|'\\b" "&#8216;" "\\b'\\|'\n\\|'\\'" "&#8217;"
    "&#8220;'" "&#8220;&#8216;" "&#8216;\"" "&#8216;&#8220;"
    "'&#8221;" "&#8217;&#8221;" "\"&#8217;" "&#8221;&#8217;")
  "Code to be automatically converted to HTML entities or other things.")

(defvar textile-error-break nil
  "Break parsing Textile when hitting a parsing error?
Do you want a total failure when you hit a textile parsing
problem?  If so then make this t.  Otherwise it defaults to nil,
and warns you in the Messages buffer.")

(defvar textile-br-all-newlines t
  "Should single newlines produce HTML linebreaks?
This variable determines whether we are using standard <br /> for
every non-block line break, or we aren't (which is nicer with the
usual way that people use Emacs, text-based modes, and
auto-fill-mode).  Right now the default is standard Textile
behavior (this could probably work with longlines.el or something
like that).")

(defvar textile-utf-16-capable
  (or (coding-system-p 'utf-16be)
      (coding-system-p 'utf-16-be)
      (coding-system-p 'utf-16-be-no-signature))
  "If we have utf-16, then we can do entity conversion.")

(defvar textile-output-to-new-buffer t
  "Should Textile output go to a new buffer?")

(defvar textile-xhtml-version-default "XHTML 1.0 Transitional"
  "What is the default version of XHTML that Textile will produce?")

(defvar textile-xhtml-docstrings
  '("XHTML 1.0 Strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
    "XHTML 1.0 Transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    "XHTML 1.0 Frameset" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">"
    "XHTML 1.1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"xhtml11-flat.dtd\">")
  "Standard HTML doctypes so that a Textile document can be self-contained.")

(defun textile-header (title &optional html-version charset &rest headers)
  "Insert HTML header so that Textile documents can be self-contained."
  (let ((my-docstrings textile-xhtml-docstrings)
        (output ""))
    (unless html-version
      (setq html-version textile-xhtml-version-default))
    (if (member html-version textile-xhtml-docstrings)
        (setq textile-xhtml-version html-version)
      (setq textile-xhtml-version textile-xhtml-version-default)
      (setq html-version textile-xhtml-version-default))
    (while my-docstrings
      (if (string= html-version (car my-docstrings))
          (setq output (cadr my-docstrings)))
      (setq my-docstrings (cddr my-docstrings)))
    (setq output (concat output "\n\n"
                         "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                         "<head>\n"
                         "<meta http-equiv=\"Content-Type\" "
                         "content=\"text/html; charset="))
    (unless charset
      (setq charset "iso-8859-1"))
    (setq output (concat output charset "\" />\n"))
    (setq output (concat output "<meta name=\"generator\" content=\""
                         textile-version ", " (car (textile-split-string
                                                    (emacs-version) "\n"))
                         "\" />\n"))
    (while headers
      (setq output (concat output (car headers) "\n"))
      (setq headers (cdr headers)))
    (setq output (concat output "<title>" title "</title>\n"))
    (setq output (concat output "</head>\n<body>"))
    output))

(defun textile-footer (&optional &rest footers)
  "Insert HTML footer so that Textile documents can be self-contained."
  (let ((output ""))
    (while footers
      (setq output (concat output (car footers) "\n"))
      (setq footers (cdr footers)))
    (setq output (concat output "</body>\n</html>"))
    output))

(defun textile-version (&optional arg)
  "Version information for this version of textile.el.
If ARG, insert string at point."
  (interactive "P")
  (if arg
      textile-version
    (message textile-version)))

(if (condition-case nil
        (split-string "a" "" t)
      (error nil))
    (defun textile-split-string (my-string separator)
      "Split MY-STRING by SEPARATOR and don't return empty substrings."
      (split-string my-string separator t))
  (defun textile-split-string (my-string separator)
    "Split MY-STRING by SEPARATOR and don't return empty substrings."
    (split-string my-string separator)))

(defun textile-string-to-list-old (my-string)
  "Process textile-encoded MY-STRING and return a textile list tree."
  (let ((old-eval-depth max-lisp-eval-depth))
    (setq max-lisp-eval-depth (+ 400 max-lisp-eval-depth))
    (setq textile-alias-list textile-alias-list-defaults)
    (setq textile-macros-list textile-macros-list-defaults)
    (prog1
        ; FIXME - blockcode-blocks doesn't accurately preserve
        ; vertical space, may have to juggle this severely
        (let ((blocks (textile-blockcode-blocks
                       (textile-escape-blocks (textile-split-string
                                               (textile-manual-pre
                                                (textile-process-elisp
                                                 (textile-process-aliases
                                                  my-string))) "\n\n+")))))
          (delete "" (mapcar 'textile-block-to-list blocks)))
      (setq max-lisp-eval-depth old-eval-depth))))

(defun textile-string-to-list (my-string)
  "Process textile-encoded MY-STRING and return a textile list tree."
  (let ((old-eval-depth max-lisp-eval-depth))
    (setq max-lisp-eval-depth (+ 100 max-lisp-eval-depth))
    (setq textile-alias-list textile-alias-list-defaults)
    (setq textile-macros-list textile-macros-list-defaults)
    (prog1
        (let ((new-list nil))
          (with-temp-buffer
            (setq case-fold-search nil)
            (insert (textile-manual-pre
                     (textile-process-elisp
                      (textile-process-aliases my-string))))
            (while (not (equal (point-min) (point-max)))
              (goto-char (point-min))
              (cond
               ((looking-at "^==>? *\n")
                (forward-char 2)
                (if (string-match ">" (match-string 0))
                ; what is the end delimiter for ==>?  Not sure yet, FIXME
                    (if (not (re-search-forward "^-\\{3,\\} *\n" nil t))
                        (re-search-forward "\n\n" nil t))
                  (replace-match "")
                  (if (not (re-search-forward "\n\\(== *\\)\\(\n\n\\)?" nil t))
                      (goto-char (point-max))
                    (replace-match "")))
                (push (buffer-substring (point-min) (point)) new-list)
                (delete-region (point-min) (point)))
               ((and (looking-at (textile-this-block-tag-regexp "bc"))
                     (string= (match-string 3) ".."))
                (let ((end-of-block
                       (catch 'found-next-block
                         (while (re-search-forward "\n\n" nil t)
                           (when (save-match-data
                                   (looking-at textile-any-block-tag-regexp))
                             (replace-match "")
                             (throw 'found-next-block (point))))
                         (point))))
                  (push (buffer-substring (point-min) end-of-block) new-list)
                  (delete-region (point-min) end-of-block)))
               (t
                (if (re-search-forward "\n\n" nil t)
                    (progn
                      (replace-match "")
                      (push (textile-block-to-list
                             (buffer-substring (point-min) (point))) new-list)
                      (delete-region (point-min) (point)))
                  (push (textile-block-to-list
                         (buffer-string)) new-list)
                  (delete-region (point-min) (point-max)))))))
          ; should I mapcar textile-block-to-list here, or earlier? FIXME
          (reverse new-list))
      (setq max-lisp-eval-depth old-eval-depth))))

(defun textile-process-elisp (my-string)
  "Find blocks marked with \"#[(())]\" and evaluate their contents."
  (with-temp-buffer
    (insert my-string)
    (goto-char (point-min))
    (while (re-search-forward "#\\[(\\(([\000-\177]+?)\\))\\]#" nil t)
      (replace-match (if noninteractive
			 "elisp evaluation not available"
                       (let ((my-response
                              (save-match-data
                                (eval (car (read-from-string
                                            (match-string 1)))))))
                         (if (stringp my-response)
                             my-response
                           (format "%S" my-response))))
                     t nil nil 0))
    (buffer-string)))

(defun textile-manual-pre (my-string)
  "If there are manually-entered <pre> blocks, escape them."
  (with-temp-buffer
    (insert my-string)
    (goto-char (point-min))
    (while (re-search-forward "^<pre>" nil t)
      (save-match-data
        (if (looking-at "\n?<code")
            (save-excursion
              (re-search-forward "<code.*?>" nil t)
              (if (looking-at "\\([\000-\177]*?\\)\\(</code>\\|</pre>\\)")
                  (replace-match (concat
                                  (save-match-data
                                    (textile-process-code (match-string 1)))
                                  (match-string 2)))))))
      (if (save-excursion
            (save-match-data
              (re-search-forward "</pre>" nil t)
              (looking-at "\n")))
          (progn
            (replace-match "==\n<pre>")
            (re-search-forward "</pre>" nil t)
            (insert "\n=="))
        (replace-match "==<pre>")
        (re-search-forward "</pre>" nil t)
        (insert "==")))
    (buffer-string)))

(defun textile-block-escape (my-string)
  "Process escaped blocks."
  (while (string-match "^==>? *\n\\{0,2\\}$" my-string)
    (setq my-string (replace-match "" nil nil my-string)))
  (list my-string (list 'textile-tag nil)))

(defun textile-escape-blocks (my-list)
  "In a list of block strings, bring escaped blocks together."
  (let ((new-list nil))
    (while my-list
      (let* ((this-item (car my-list)))
        (setq my-list (cdr my-list))
        (if (string-match "^== *\n" this-item)
            (let* ((escape-block (replace-match "" nil nil this-item))
                   (hit-end (string-match "\n== *$" escape-block)))
              (if hit-end
                  (setq escape-block (replace-match "" nil nil escape-block)))
              (while (and my-list (not hit-end))
                (if (string-match "\n== *$" (car my-list))
                    (progn
                      (setq escape-block
                            (concat escape-block "\n\n"
                                    (replace-match "" nil nil
                                                   (car my-list))))
                      (setq hit-end t))
                  (setq escape-block (concat escape-block "\n\n"
                                             (car my-list))))
                (setq my-list (cdr my-list)))
              (push (list escape-block (list 'textile-tag nil)) new-list))
          (if (string-match "^==> *$" this-item)
              (let* ((escape-block (replace-match "" nil nil this-item))
                     (hit-end (string-match "^----- *$" escape-block)))
                (if hit-end
                    (setq escape-block
                          (replace-match "" nil nil escape-block)))
                (while (and my-list (not hit-end))
                  (if (string-match "^----- *$" (car my-list))
                      (progn
                        (setq escape-block
                              (concat escape-block "\n\n"
                                      (replace-match "" nil nil
                                                     (car my-list))))
                        (setq hit-end t))
                    (setq escape-block (concat escape-block "\n\n"
                                               (car my-list))))
                  (setq my-list (cdr my-list)))
                (push (list escape-block (list 'textile-tag nil)) new-list))
            (push this-item new-list)))))
    (reverse new-list)))

(defun textile-block-blockcode (my-string)
  "Process blockcode block."
  (textile-blockcode-blocks (textile-split-string my-string "\n\n")))

(defun textile-blockcode-blocks (my-list)
  "In a list of block strings, bring blockcode blocks together."
  (let ((new-list nil))
    (while my-list
      (let* ((this-item (car my-list)))
        (setq my-list (cdr my-list))
        (if (and (stringp this-item)
                 (string-match (textile-this-block-tag-regexp "bc") this-item))
            (let* ((my-tag (match-string 1 this-item))
                   (my-attr (match-string 2 this-item))
                   (extended-p (match-string 3 this-item))
                   (code-block (textile-process-code
                                (replace-match "" nil nil this-item)))
                   (hit-end nil))
              (if (string= extended-p "..")
                  (setq code-block (concat code-block "\n\n"
                                           (mapconcat 'textile-process-code
                                                      my-list "\n\n"))))
              (push (list (list code-block (list 'textile-tag "code"))
                          (plist-put my-attr 'textile-tag "pre"))
                    new-list))
          (push this-item new-list))))
    (reverse new-list)))

(defun textile-block-to-list (my-string)
  "Process textile-encoded MY-STRING and return a textile block tree."
  (if (listp my-string)
      my-string
    (let ((my-plist (plist-put nil 'textile-explicit t)))
      (cond
       ; test for block tags
       ((string-match "^==>? *\n" my-string)
        (textile-block-escape my-string))
       ((string-match (textile-this-block-tag-regexp "bc") my-string)
        (textile-block-blockcode my-string))
       ((string-match "^clear[<>]?\\. *$" my-string)
        (setq my-string (textile-block-clear my-string)))
       ((and
         (string-match "^.*?|.*| *$" my-string)
         (not (string-match "^table" my-string)))
        (textile-block-table my-string nil))
       ((string-match textile-block-tag-regexp my-string)
        (let* ((tag (match-string 1 my-string))
               (attributes 
                (if (string= tag "table")
                    (textile-attributes " " (match-string 2 my-string)
                                        'table-outer-tag)
                  (textile-attributes " " (match-string 2 my-string))))
               ; FIXME - apparently BC allows for multi-line style
               ; assignment, meaning you can have newline chars within
               ; attribute strings... no idea what I'm going to do with
               ; that...
               (extended (string= (match-string 3 my-string) ".."))
               (my-function
                (car (read-from-string (concat "textile-block-" tag)))))
          (setq attributes (plist-put attributes 'textile-extended extended))
          (setq attributes (plist-put attributes 'textile-explicit t))
          (cond
           ((fboundp my-function)
            (funcall my-function my-string attributes))
           ((string-match "^h[1-6]$" tag)
            (setq attributes (plist-put attributes 'textile-hlevel
                                        (substring tag 1 2)))
            (textile-block-header my-string attributes))
           ((string-match "^fn[0-9]+$" tag)
            (setq attributes (plist-put attributes 'textile-fn-num
                                        (substring tag 2)))
            (setq attributes (plist-put attributes 'class "footnote"))
            (setq attributes (plist-put attributes 'id tag))
            (textile-block-footnote my-string attributes))
           (t
            (setq my-plist (plist-put my-plist 'textile-tag "p"))
            (setq my-plist (plist-put my-plist 'textile-explicit nil))
            (list (textile-inline-to-list my-string) my-plist)))))
       ((string-match textile-list-tag-regexp my-string)
        (textile-block-list my-string))
       (t
        (setq my-plist (plist-put my-plist 'textile-tag "p"))
        (setq my-plist (plist-put my-plist 'textile-explicit nil))
        (list (textile-inline-to-list my-string) my-plist))))))

(defun textile-inline-to-list (my-string)
  "Process textile-encoded MY-STRING and return a textile inline tree."
  (save-match-data
    (let ((my-plist (plist-put nil 'textile-tag nil))
          (my-list (list my-string)))
      (cond
     ; break it up for inline tags
       )
    ; turn my-string into a list of strings
      (setq my-list (mapcar 'textile-encode-escapes my-list))
      (setq my-list (mapcar 'textile-encode-tags my-list))
      (setq my-list (mapcar 'textile-encode-manual my-list))
      ; character-replacement processing
      (setq my-list (mapcar 'textile-process-quotes my-list))
      (setq my-list (mapcar 'textile-process-macros my-list))
      ; tag-handling processing
      (setq my-list (mapcar 'textile-decode-manual my-list))
      (setq my-list (mapcar 'textile-process-inline my-list))
      (setq my-list (mapcar 'textile-decode-tags my-list))
      (setq my-list (mapcar 'textile-process-image my-list))
      (setq my-list (mapcar 'textile-process-footnote my-list))
      (setq my-list (mapcar 'textile-process-link my-list))
      (setq my-list (mapcar 'textile-process-acronym my-list))
      (setq my-list (mapcar 'textile-process-auto-link my-list))
      (setq my-list (mapcar 'textile-process-ampersand my-list))
    ; from this point on there will be no more converting ampersands
    ; to &amp;
      (setq my-list (mapcar 'textile-process-newline my-list))
      (if textile-utf-16-capable
          (setq my-list (mapcar 'textile-process-non-ascii my-list)))
      (setq my-list (mapcar 'textile-decode-escapes my-list))
      (append my-list (list my-plist)))))

(defmacro textile-skip-tags (my-function my-string &rest body)
  "If MY-STRING is a list, then perform BODY on each member of MY-STRING."
  (list 'if
        (list 'listp my-string)
        (list 'if (list 'member ''textile-tag my-string)
              my-string
              (list 'mapcar my-function my-string))
        (cons 'progn body)))

; add edebug instrumentation for lisp macro(s)
(add-hook 'edebug-setup-hook
          (function (lambda ()
                      (def-edebug-spec textile-skip-tags
                        (function-form sexp body)))))

(defun textile-process-macros (my-string)
  "Convert any macros found in MY-STRING to their equivalent entities."
  (textile-skip-tags 'textile-process-macros my-string
                     (with-temp-buffer
                       (setq case-fold-search t)
                       (insert my-string)
                       (let ((my-macros textile-macros-list))
                         (while my-macros
                           (let ((search-for (car my-macros))
                                 (replace-with (cadr my-macros)))
                             (goto-char (point-min))
                             (while (re-search-forward search-for nil t)
                               (replace-match replace-with))
                             (setq my-macros (cddr my-macros)))))
                       (buffer-string))))

(defun textile-process-newline (my-string)
  "Convert newlines to <br /> tags if so desired."
  (if textile-br-all-newlines
      (textile-skip-tags 'textile-process-newline my-string
                         (with-temp-buffer
                           (insert my-string)
                           (goto-char (point-min))
                           (while (re-search-forward "\n" nil t)
                             (replace-match "<br />\n"))
                           (buffer-string)))
    my-string))

(defun textile-process-ampersand (my-string)
  "Convert ampersands to &amp; as necessary."
  (textile-skip-tags 'textile-process-ampersand my-string
                     (with-temp-buffer
                       (insert my-string)
                       (goto-char (point-min))
                       (save-excursion
                         (while (re-search-forward "&" nil t)
                           (if (looking-at "#?\\w+;")
                               (re-search-forward "#?\\w+;" nil t)
                             (replace-match "&amp;"))))
                       (buffer-string))))

(defun textile-process-code (my-string)
  "Process necessary things in <code> fragments, like the five XML entities."
  ; ugly hack but easier than the alternative right now
  (textile-skip-tags 'textile-process-code my-string
                     (with-temp-buffer
                       (insert (textile-process-ampersand my-string))
                       (goto-char (point-min))
                       (save-excursion
                         (while (re-search-forward "<" nil t)
                           (replace-match "&lt;")))
                       (save-excursion
                         (while (re-search-forward ">" nil t)
                           (replace-match "&gt;")))
                       (save-excursion
                         (while (re-search-forward "\"" nil t)
                           (replace-match "&quot;")))
                       (save-excursion
                         (while (re-search-forward "'" nil t)
                           (replace-match "&apos;")))
                       (buffer-string))))

(defun textile-process-acronym (my-string)
  "Process all acronyms in a given string or list of strings."
  (if (and (listp my-string)
           (listp (car (last my-string)))
           (textile-get-attributes my-string)
           (plist-get (textile-get-attributes my-string) 'textile-tag)
           (or (string=
                (plist-get (textile-get-attributes my-string) 'textile-tag)
                "acronym")
               (and
                (string=
                 (plist-get (textile-get-attributes my-string) 'textile-tag)
                 "span")
                (string=
                 (plist-get (textile-get-attributes my-string) 'textile-tag)
                 "caps"))))
      my-string
    (textile-skip-tags
     'textile-process-acronym
     my-string
     ; FIXME - BC allows some punctuation in caps spans:
     ; testcases.txt, line 595
     (if (string-match
          "\\<\\([A-Z]\\{3,\\}\\|[0-9][A-Z]\\{2,\\}\\|[A-Z][0-9A-Z]\\{2,\\}\\)\\((\\(.*?\\))\\|\\)"
          my-string)
         (if (not (equal (match-beginning 0) 0))
             (list (substring my-string 0 (match-beginning 0))
                   (textile-process-acronym (substring my-string
                                                       (match-beginning 0)))
                   (plist-put nil 'textile-tag nil))
           (if (not (equal (match-end 0) (length my-string)))
               (list (save-match-data
                       (textile-process-acronym (substring my-string
                                                           (match-beginning 0)
                                                           (match-end 2))))
                     (textile-process-acronym (substring my-string
                                                         (match-end 2)))
                     (plist-put nil 'textile-tag nil))
             (list
              (match-string 1 my-string)
              (if (match-string 3 my-string)
                  (plist-put (plist-put 'nil 'title (match-string 3 my-string))
                             'textile-tag "acronym")
                (plist-put (plist-put 'nil 'class "caps")
                           'textile-tag "span")))))
       my-string))))

(defun textile-encode-manual (my-string)
  "Tokenize manual HTML tags."
  (textile-skip-tags 'textile-encode-manual my-string
    (while (string-match
            "\\(\\(<code.*?>\\)\\(.*?\\)\\(</code>\\)\\|<a .*?>\\|</a>\\)"
            my-string)
      (if (match-string 3 my-string)
          (push (concat (match-string 2 my-string)
                        (save-match-data
                          (textile-process-code (match-string 3 my-string)))
                        (match-string 4 my-string)) Textile-manual)
        (push (match-string 0 my-string) Textile-manual))
      (setq my-string
            (replace-match
             (format "emacs_textile_manual_token_%0d_x"
                     (safe-length Textile-manual))
             nil nil my-string)))
    my-string))

(defun textile-decode-manual (my-string)
  "Find tokens and replace them with their original contents."
  (textile-skip-tags 'textile-decode-manual my-string
    (while (string-match "emacs_textile_manual_token_\\([0-9]+\\)_x"
                         my-string)
      (setq my-string
            (replace-match
             (nth (- (safe-length Textile-manual)
                     (string-to-number (match-string 1 my-string)))
                  Textile-manual) nil t my-string)))
    my-string))

(defun textile-encode-tags (my-string)
  "Tokenize links, images, footnotes, and acronyms."
  (textile-skip-tags
   'textile-encode-tags
   my-string
    ; links
   (while (string-match
           "\\(\"[^\"]*?\":[^ ]*?\\)\\([],.;:\"']?\\(?: \\|$\\)\\)"
           my-string)
     (push (match-string 1 my-string) Textile-tags)
     (setq my-string
           (replace-match
            (format "emacs_textile_tag_token_%0d_x\\2"
                    (safe-length Textile-tags))
            nil nil my-string)))
    ; images
   (while (string-match
           "\\(!\\([^ ][^!]+?\\) *\\((\\(.*?\\))\\)?!\\)\\(:\\([^ ]*?\\)\\([,.;:]?\\(?: \\|$\\)\\)\\)?"
           my-string)
     (if (match-string 5 my-string)
         (progn
           (push (concat (match-string 1 my-string)
                         ":"
                         (match-string 6 my-string)) Textile-tags)
           (setq my-string
                 (replace-match
                  (format "emacs_textile_tag_token_%0d_x\\7"
                          (safe-length Textile-tags))
                  nil nil my-string)))
       (push (match-string 1 my-string) Textile-tags)
       (setq my-string
             (replace-match
              (format "emacs_textile_tag_token_%0d_x"
                      (safe-length Textile-tags))
              nil nil my-string))))
    ; footnotes
   (while (string-match "\\[\\([0-9]+\\)\\]" my-string)
     (push (match-string 0 my-string) Textile-tags)
     (setq my-string
           (replace-match
            (format "emacs_textile_tag_token_%0d_x"
                    (safe-length Textile-tags))
            nil nil my-string)))
   my-string))

(defun textile-decode-tags (my-string)
  "Find tokens and replace them with their original contents."
  (textile-skip-tags
   'textile-decode-tags
   my-string
   (while (string-match "emacs_textile_tag_token_\\([0-9]+\\)_x" my-string)
     (setq my-string
           (replace-match
            (nth (- (safe-length Textile-tags)
                    (string-to-number (match-string 1 my-string)))
                 Textile-tags) nil t my-string)))
   my-string))

(defun textile-encode-escapes (my-string)
  "Tokenize escaped strings and store the tokens in Textile-escapes."
  (textile-skip-tags
   'textile-encode-escapes
   my-string
   (while (string-match
           "\\(^\\|\\W\\)\\(==\\([^\000]+?\\)==\\)\\(\\W\\|\\$\\)"
           my-string)
     (push (match-string 3 my-string) Textile-escapes)
     (setq my-string
           (replace-match
            (format "\\1emacs_textile_token_%0d_x\\4"
                    (safe-length Textile-escapes))
            nil nil my-string)))
   my-string))

(defun textile-decode-escapes (my-string)
  "Find tokens and replace them with escaped strings."
  (textile-skip-tags
   'textile-decode-escapes
   my-string
    (while (string-match "emacs_textile_token_\\([0-9]+\\)_x" my-string)
      (setq my-string
            (replace-match
             (nth (- (safe-length Textile-escapes)
                     (string-to-number (match-string 1 my-string)))
                  Textile-escapes) nil t my-string)))
    my-string))

(defun textile-process-inline (my-string)
  "Process all of the usual inline tags in a given string or list of strings."
  (textile-skip-tags 'textile-process-inline my-string
      (if (string-match textile-inline-tag-regexp my-string)
          (if (not (equal (match-beginning 0) 0))
              (list (textile-remove-braces
                     (substring my-string 0 (match-beginning 1)))
                    (save-match-data
                      (textile-process-inline (substring my-string
                                                         (match-beginning 1))))
                    (plist-put nil 'textile-tag nil))
            (if (not (equal (match-end 3) (length my-string)))
                (list (save-match-data
                        (textile-process-inline (substring my-string
                                                           (match-beginning 1)
                                                           (match-end 3))))
                      (save-match-data
                        (textile-process-inline
                         (textile-remove-braces
                          (substring my-string
                                     (match-end 3)))))
                      (plist-put nil 'textile-tag nil))
              (let* ((attributes (plist-put
                                  (save-match-data
                                    (textile-attributes "[^})]]"
                                                        (match-string
                                                         2 my-string)
                                                        'inline))
                                  'textile-tag  (textile-generate-inline-tag
                                                 (match-string 1 my-string)
                                                 textile-inline-tag-list)))
                     (text (substring
                            (match-string 2 my-string)
                            (length (plist-get attributes
                                               'textile-attrib-string)))))
                (if (string= (match-string 1 my-string) "@")
                    (list (textile-process-code text)
                          attributes)
                  (list (textile-process-inline text)
                        attributes)))))
        my-string)))

(defun textile-remove-braces (my-string)
  "Remove inappropriate braces from the beginning or end of a string."
  (if (string= my-string "")
      my-string
    (save-match-data
      (textile-skip-tags
       'textile-remove-braces
       my-string
       (if (string= (substring my-string 0 1) "]")
           (setq my-string (substring my-string 1)))
       (if (and (not (string= my-string ""))
                (string= (substring my-string
                                    (- (length my-string) 1)
                                    (length my-string)) "["))
           (setq my-string (substring my-string 0 (- (length my-string) 1))))
       my-string))))

(defun textile-process-footnote (my-string)
  "Process all footnotes in a given string or list of strings."
  (textile-skip-tags 'textile-process-footnote my-string
    (if (string-match "\\[\\([0-9]+\\)\\]" my-string)
        (if (not (equal (match-beginning 0) 0))
            (list (substring my-string 0 (match-beginning 0))
                  (save-match-data
                    (textile-process-footnote (substring my-string
                                                         (match-beginning 0)
                                                         (match-end 0))))
                  (plist-put nil 'textile-tag nil))
          (if (not (equal (match-end 0) (length my-string)))
              (list (save-match-data
                      (textile-process-footnote (substring my-string
                                                           (match-beginning 0)
                                                           (match-end 0))))
                    (save-match-data
                      (textile-process-footnote (substring my-string
                                                           (match-end 0))))
                    (plist-put nil 'textile-tag nil))
            (list
             (list
              (match-string 1 my-string)
              (plist-put
               (plist-put 'nil 'textile-tag "a")
               'href (concat "#fn" (match-string 1 my-string))))
             (plist-put
              (plist-put 'nil 'textile-tag "sup")
              'class "footnote"))))
      my-string)))

(defun textile-process-image (my-string)
  "Process all images in a given string or list of strings."
  (textile-skip-tags 'textile-process-image my-string
    (if (string-match
         "!\\([^ ][^!]+?\\) *\\((\\(.*?\\))\\)?!\\(?::\\([^ ]*?\\)\\([,.;:]?\\(?: \\|$\\)\\)\\)?" my-string)
        (if (not (equal (match-beginning 0) 0))
            (list (substring my-string 0 (match-beginning 0))
                  (textile-process-image (substring my-string
                                                    (match-beginning 0)))
                  (plist-put nil 'textile-tag nil))
          (if (not (equal (match-end 0) (length my-string)))
              (list (save-match-data
                      (textile-process-image (substring my-string
                                                        (match-beginning 0)
                                                        (match-end 0))))
                    (textile-process-image (substring my-string
                                                      (match-end 0)))
                    (plist-put nil 'textile-tag nil))
            (let* ((image-data (match-string 1 my-string))
                   (title (match-string 3 my-string))
                   (url (match-string 4 my-string))
                   (delimiter (match-string 5 my-string))
                   (alias-info (textile-alias-to-url url textile-alias-list)))
              (if url
                  (let ((link-title nil))
                    (if alias-info
                        (progn
                          (setq link-title (car alias-info))
                          (setq url (cadr alias-info))))
                    (if (string= link-title "")
                        (setq link-title nil))
                    (list (list (textile-string-to-image image-data title)
                                (plist-put
                                 (plist-put
                                  (plist-put nil 'textile-tag "a")
                                  'href (textile-process-ampersand url))
                                 'title link-title)) delimiter
                                 (plist-put nil 'textile-tag nil)))
                (textile-string-to-image image-data title)))))
      my-string)))

(defun textile-string-to-image (my-string title)
  "Process MY-STRING and return an inline image tree."
  (let* ((attributes (textile-attributes "[^})]]" my-string 'image))
         (my-string (substring my-string
                               (length (plist-get attributes
                                                  'textile-attrib-string))))
         (my-list (textile-split-string my-string " "))
         (my-dimensions (if (cadr my-list)
                            (textile-split-string (cadr my-list) "x")
                          nil)))
    (setq attributes (plist-put attributes 'textile-tag "img"))
    (setq attributes (plist-put attributes 'alt title))
    (setq attributes (plist-put attributes 'src (car my-list)))
    (if (> (safe-length my-dimensions) 1)
        (progn
          (setq attributes (plist-put attributes 'width (car my-dimensions)))
          (setq attributes (plist-put attributes 'height
                                      (cadr my-dimensions))))
      (dolist (this-parm (cdr my-list))
        (if (string-match "w$" this-parm)
            (setq attributes (plist-put attributes 'width
                                        (substring this-parm 0
                                                   (- (length this-parm) 1))))
          (if (string-match "h$" this-parm)
              (setq attributes (plist-put attributes 'height
                                          (substring this-parm 0
                                                     (- (length this-parm)
                                                        1))))))))
    (list "" attributes)))

(defun textile-process-quotes (my-string)
  "Educate all quotes in a given string or list of strings."
  (textile-skip-tags
   'textile-process-quotes
   my-string
   (with-temp-buffer
     (insert my-string)
     (goto-char (point-min))
     (let ((links nil)
           (textile-quotes-list textile-smart-quotes-list))
       (while
           (re-search-forward
            "\\(\"\\([^\"]*?\\)\":\\([^ ]*?\\)\\)\\([,.;:\"']?\\( \\|$\\)\\)"
            nil t)
         (push (match-string 1) links)
         (replace-match
          (format "emacs_textile_link_token_%0d_x%s" (safe-length links)
                  (match-string 4))))
       (goto-char (point-min))
       (while (re-search-forward "\\b\\(@.*?@\\)\\b" nil t)
         (push (match-string 1) links)
         (replace-match
          (format "emacs_textile_link_token_%0d_x" (safe-length links))))
       (goto-char (point-min))
       (while textile-quotes-list
         (save-excursion
           (while (re-search-forward (car textile-quotes-list) nil t)
             (replace-match (cadr textile-quotes-list)))
           (setq textile-quotes-list (cddr textile-quotes-list))))
       (while (re-search-forward "emacs_textile_link_token_\\([0-9]+\\)_x"
                                 nil t)
         (replace-match (nth (- (safe-length links)
                                (string-to-number (match-string 1))) links))))
     (buffer-string))))

(defun textile-process-link (my-string)
  "Process all links in a given string or list of strings."
  (textile-skip-tags 'textile-process-link my-string
    (if (or
         (string-match
          "\"\\([^\"]*?\\)\":\\([^ ]*?\\)\\(&#[0-9]+;\\)"
          my-string)
         (string-match
          "\"\\([^\"]*?\\)\":\\([^ ]*?\\)\\([],.;:\"']?\\(?: \\|$\\)\\)"
          my-string))
        (if (not (equal (match-beginning 0) 0))
            (list (textile-remove-braces
                   (substring my-string 0 (match-beginning 0)))
                  (textile-process-link (substring my-string
                                                   (match-beginning 0)))
                  (plist-put nil 'textile-tag nil))
          (if (not (equal (match-end 0) (length my-string)))
              (list (save-match-data
                      (textile-process-link (substring my-string
                                                       (match-beginning 0)
                                                       (match-end 3))))
                    (textile-process-link
                      (substring my-string
                                 (match-end 3)))
                    (plist-put nil 'textile-tag nil))
            (let* ((text (match-string 1 my-string))
                   (url (match-string 2 my-string))
                   (delimiter (textile-remove-braces
                               (match-string 3 my-string)))
                   (title "")
                   (alias-info (textile-alias-to-url url textile-alias-list)))
              (if alias-info
                  (progn
                    (setq title (car alias-info))
                    (setq url (cadr alias-info)))
                (if (string-match "\\(.*\\) +(\\(.*\\))" text)
                    (progn
                      (setq title (match-string 2 text))
                      (setq text (match-string 1 text)))))
              (if (string= title "")
                  (setq title nil))
              (list (list (textile-inline-to-list text)
                          (plist-put
                           (plist-put
                            (plist-put nil 'textile-tag "a")
                            'href (textile-process-ampersand url))
                           'title title)) delimiter
                           (plist-put nil 'textile-tag nil)))))
      my-string)))

(defun textile-process-auto-link (my-string)
  (textile-skip-tags
   'textile-process-auto-link
   my-string
    (if (string-match
         "\\(\\(?:http\\|ftp\\|mailto\\):\\(?://\\)?.*?\\)\\([],.;:\"']?\\(?: \\|$\\)\\)"
         my-string)
        (if (not (equal (match-beginning 0) 0))
            (list (textile-remove-braces
                   (substring my-string 0 (match-beginning 0)))
                  (textile-process-auto-link (substring my-string
                                                        (match-beginning 0)))
                  (plist-put nil 'textile-tag nil))
          (if (not (equal (match-end 0) (length my-string)))
              (list (save-match-data
                      (textile-process-auto-link (substring my-string
                                                            (match-beginning 0)
                                                            (match-end 2))))
                    (textile-process-auto-link
                     (substring my-string
                                (match-end 2)))
                    (plist-put nil 'textile-tag nil))
            (list (list (textile-process-ampersand (match-string 1 my-string))
                        (plist-put
                         (plist-put nil 'textile-tag "a")
                         'href (textile-process-ampersand
                                (match-string 1 my-string))))
                  (match-string 2 my-string)
                  (plist-put nil 'textile-tag nil))))
      my-string)))

(defun textile-process-non-ascii (my-string)
  (textile-skip-tags 'textile-process-non-ascii my-string
    (while (string-match "\\([^\000-\177]+\\)" my-string)
      (let* ((non-ascii-string (match-string 1 my-string))
             (replacement (save-match-data
                            (textile-non-ascii-to-unicode non-ascii-string))))
        (setq my-string (replace-match replacement nil nil my-string))))
    my-string))

(defun textile-list-to-blocks (my-list)
  "Convert list of textile trees to XHTML string."
  (textile-final-XHTML-tidy
   (mapconcat 'textile-compile-string my-list "\n\n")))

(defun textile-append-block (my-list block)
  "Insert BLOCK into MY-LIST as the second-to-last item, and return MY-LIST."
  (setcdr (nthcdr (- (safe-length my-list) 2) my-list)
          (cons block (nthcdr (- (safe-length my-list) 1) my-list)))
  my-list)

(defun textile-unextend-blocks (my-list)
  "In a list of textile trees, pull extended blocks together."
  ; FIXME - this doesn't seem to pass testcases.txt line 285
  (let ((new-list nil))
    (while my-list
      (let* ((this-item (car my-list))
             (my-attr (car (last this-item))))
        (setq my-list (cdr my-list))
        (if (plist-get my-attr 'textile-extended)
            (while (and my-list
                        (not (plist-get (car (last (car my-list)))
                                        'textile-explicit)))
              (textile-append-block this-item (car my-list))
              (setq my-list (cdr my-list))))
        (push this-item new-list)))
    (setq new-list (reverse new-list))
    (while new-list
      (let* ((this-item (car new-list))
             (my-attr (car (last this-item)))
             (clear-info (plist-get my-attr 'next-block-clear)))
        (setq new-list (cdr new-list))
        (if clear-info
            (if new-list
                (progn
                  (setq this-item (reverse (car new-list)))
                  (setq my-attr (plist-put (car this-item) 'style
                                           (concat (plist-get (car this-item)
                                                              'style)
                                                   clear-info)))
                  (setcar this-item my-attr)
                  (push (reverse this-item) my-list)
                  (setq new-list (cdr new-list))))
          (push this-item my-list))))
    (reverse my-list)))

(defun textile-compile-string (my-list)
  "Convert textile tree to XHTML string."
  (if (stringp my-list)
      my-list
    (let* ((my-plist (car (last my-list)))
           (args (textile-all-but-last  my-list))
           (my-string (mapconcat (function
                                  (lambda (item)
                                    (if (stringp item)
                                        item
                                      (textile-compile-string item))))
                                 args "")))
      (if (and (listp my-plist) (plist-get my-plist 'textile-tag))
          (textile-enclose-tag my-string my-plist)
        my-string))))

(defun textile-final-XHTML-tidy (my-string)
  "Final pretty-printing details after all processing is done."
  (while (string-match "<br />\\(.\\)" my-string)
    (setq my-string (replace-match "<br />\n\\1" nil nil my-string)))
  (while (string-match "</tr>\\(.\\)" my-string)
    (setq my-string (replace-match "</tr>\n\\1" nil nil my-string)))
  (while (string-match "</p><p" my-string)
    (setq my-string (replace-match "</p>\n\n<p" nil nil my-string)))
  (while (string-match "\\(.\\)<\\(ul\\|ol\\)" my-string)
    (setq my-string (replace-match "\\1\n<\\2" nil nil my-string)))
  (while (string-match "<\\(/li\\|/?ol\\|/?ul\\)>\\(.\\)" my-string)
    (setq my-string (replace-match "<\\1>\n\\2" nil nil my-string)))
  (while (string-match "\\(.\\)\\(<li[ >]\\)" my-string)
    (setq my-string (replace-match "\\1\n\\2" nil nil my-string)))
  my-string)

(defun textile-process-aliases (my-string)
  "Return MY-STRING without aliases, and set textile-alias-list."
  (while (string-match "\\(^\\|\n\\)\\[\\(.*?\\)\\]\\([^\n]+\\)\n"
                       my-string)
    (let* ((delimiter (match-string 1 my-string))
           (alias-string (match-string 2 my-string))
           (url-string (match-string 3 my-string))
           (alias "")
           (title ""))
      (setq my-string (replace-match delimiter nil nil my-string))
      (if (string-match "\\(.*\\) +(\\(.*\\))" alias-string)
          (progn
            (setq alias (match-string 1 alias-string))
            (setq title (match-string 2 alias-string)))
        (setq alias alias-string))
      (if (member alias textile-alias-list)
          (dotimes (i (safe-length textile-alias-list))
            (if (and
                 (stringp (nth i textile-alias-list))
                 (string= (nth i textile-alias-list) alias))
                (setcar (nthcdr (1+ i) textile-alias-list)
                        (if (string= title "")
                            url-string
                          (list title url-string)))))
        (setq textile-alias-list
              (cons
               (if (string= title "")
                   url-string
                 (list title url-string))
               textile-alias-list))
        (setq textile-alias-list
              (cons alias textile-alias-list)))))
  my-string)

(defun textile-attributes (&optional stop-regexp attrib-string &rest context)
  "Return a plist of attributes from (point) or ATTRIB-STRING.
If ATTRIB-STRING is non-nil, then make a new buffer with that;
otherwise make a new buffer with the entirety of the buffer
from (point) to the end.  Process it until reaching a space that
isn't within some kind of attribute block.  While processing,
handle different kinds of attributes, including styles, classes,
ids, and langs.  Stop processing at the end of the buffer, string,
or STOP-REGEXP."
   (let ((my-plist nil)
        (style (if (memq 'next-block-clear context)
                   (list (plist-get context 'next-block-clear))
                 nil))
        (class nil)
        (id nil)
        (lang nil)
        (left-pad 0)
        (right-pad 0)
        (align nil)
        (valign nil)
        (rowspan nil)
        (colspan nil)
        (textile-header nil)
        (not-finished t)
        (textile-well-formed t)
        (stop-regexp (or stop-regexp
                         " "))
        (attrib-string (or attrib-string
                           (buffer-substring (point) (point-max)))))
     (with-temp-buffer
       (insert 
        (or attrib-string ""))
       (goto-char (point-min))
       (while not-finished
         (let ((this-char (char-after)))
           (cond
            ((eobp)
             (setq not-finished nil))
            ((looking-at stop-regexp)
             (re-search-forward stop-regexp nil t)
             (setq not-finished nil))
            ((looking-at "{\\([^}]*\\)}")
             (push (match-string 1) style)
             (re-search-forward "}" nil t))
            ((and
              (looking-at "\\[\\(.*?\\)\\]")
              (not (memq 'inline context)))
             (setq lang (match-string 1))
             (re-search-forward "\\]" nil t))
            ((looking-at "(\\([^) (]+\\))")
             (let ((this-attrib (save-match-data
                                  (textile-split-string
                                   (match-string 1) "#"))))
               (if (and (string-match "#" (match-string 1))
                        (= (match-beginning 0) 0))
                   (setq id (car this-attrib))
                 (push (car this-attrib) class)
                 (setq id (cadr this-attrib))))
             (re-search-forward ")" nil t))
            ((looking-at "(.* .*)")
             ; parenthetical statement including a space
             (setq not-finished nil))
            ((and (not (memq 'inline context))
                  (equal this-char ?\())
             (setq left-pad (1+ left-pad))
             (forward-char 1))
            ((and (not (memq 'inline context))
                  (equal this-char ?\)))
             (setq right-pad (1+ right-pad))
             (forward-char 1))
            ((and (not (memq 'inline context))
                  (equal this-char ?\>))
             (if (memq 'table context)
                 (setq align "right")
               (push "right" class)
               (push "text-align: right" style))
             (forward-char 1))
            ((and (not (memq 'inline context))
                  (looking-at "<>"))
             (push "justify" class)
             (push "text-align: justify" style)
             (forward-char 2))
            ((and (not (memq 'inline context))
                  (equal this-char ?\<))
             (push "left" class)
             (if (memq 'table context)
                 (setq align "left")
               (push "text-align: left" style))
             (forward-char 1))
            ((and (not (memq 'inline context))
                  (equal this-char ?\=))
             (cond
              ((memq 'table context)
               (setq align "center"))
              ((memq 'table-outer-tag context)
               (push "margin-left: auto" style)
               (push "margin-right: auto" style))
              (t
               (push "center" class)
               (push "text-align: center" style)))
             (forward-char 1))
            ((and (memq 'table context)
                  (equal this-char ?^ ))
             (setq valign "top")
             (forward-char 1))
            ((and (memq 'image context)
                  (equal this-char ?^ ))
             (push "vertical-align: text-top" style))
            ((and (memq 'table context)
                  (equal this-char ?\~))
             (setq valign "bottom")
             (forward-char 1))
            ((and (memq 'image context)
                  (equal this-char ?\~ ))
             (push "vertical-align: text-bottom" style))
            ((and (memq 'image context)
                  (equal this-char ?-))
             (push "vertical-align: middle" style))
            ((and (memq 'table context)
                  (looking-at "[\\]\\([0-9]+\\)"))
             (setq colspan (match-string 1))
             (re-search-forward "[\\]\\([0-9]+\\)" nil t))
            ((and (memq 'table context)
                  (looking-at "/\\([0-9]+\\)"))
             (setq rowspan (match-string 1))
             (re-search-forward "/\\([0-9]+\\)" nil t))
            ((and
              (equal this-char ?\_)
              (or
               (memq 'table context)
               (memq 'table-outer-tag context)))
             (setq textile-header t)
             (forward-char 1))
            (t
            ; if you hit something you don't recognize, then this
            ; isn't an attribute string
             (setq textile-well-formed nil)
             (setq not-finished nil)))))
       (setq my-plist (plist-put my-plist 'textile-attrib-string
                                 (buffer-substring (point-min)
                                                   (point)))))
     (if (> left-pad 0)
         (push (concat "padding-left: " (format "%d" left-pad) "em") style))
     (if (> right-pad 0)
         (push (concat "padding-right: " (format "%d" right-pad) "em")
               style))
     (when (and (or (> left-pad 0) (> right-pad 0))
                style)
       (when (member "text-align: left" style)
         (setq style (delete "text-align: left" style))
         (push "float: left" style))
       (when (member "text-align: right" style)
         (setq style (delete "text-align: right" style))
         (push "float: right" style)))
     (dolist (this-variable '(style class id lang align valign
                                    colspan rowspan
                                    textile-header textile-well-formed))
       (when (and (stringp (eval this-variable))
                  (string= (eval this-variable) ""))
         (set this-variable nil))
       (setq my-plist (plist-put my-plist
                                 (if (and
                                      (eq this-variable 'lang)
                                      (string=
                                       textile-xhtml-version "XHTML 1.1"))
                                     'xml:lang
                                   this-variable)
                                 (eval this-variable))))
     my-plist))

(defun textile-non-ascii-to-unicode (string)
  "Convert STRING to Unicode entities."
  (cond
   ((coding-system-p 'utf-16be) ; Emacs 21.4
    (let ((unicode-string (encode-coding-string string
                                                'utf-16be))
          (unicode-values nil)
          (output ""))
      (setq unicode-values (mapcar 'string-to-char
                                   (textile-split-string
                                    unicode-string "")))
      (while (cdr unicode-values)
        (setq output (concat output "&#" (number-to-string
                                          (+ (* (car unicode-values) 256)
                                             (cadr unicode-values)))
                             ";"))
        (setq unicode-values (cddr unicode-values)))
      output))
   ((coding-system-p 'utf-16-be) ; Emacs 21.3
    (let ((unicode-string (encode-coding-string string 'utf-16-be))
          (unicode-values nil)
          (output ""))
      (setq unicode-values (mapcar 'string-to-char
                                   (nthcdr 2
                                           (textile-split-string
                                            unicode-string ""))))
      (while (cdr unicode-values)
        (setq output (concat output "&#" (number-to-string
                                          (+ (* (car unicode-values) 256)
                                             (cadr unicode-values)))
                             ";"))
        (setq unicode-values (cddr unicode-values)))
      output))
   ((coding-system-p 'utf-16-be-no-signature) ; Mule-UCS
    (let ((unicode-string (encode-coding-string string
                                                'utf-16-be-no-signature))
          (unicode-values nil)
          (output ""))
      (setq unicode-values (mapcar 'string-to-char (textile-split-string
                                                    unicode-string "")))
      (while (cdr unicode-values)
        (setq output (concat output "&#" (number-to-string
                                          (+ (* (car unicode-values) 256)
                                             (cadr unicode-values)))
                             ";"))
        (setq unicode-values (cddr unicode-values)))
      output))
   (t
    string)))

(defun textile-alias-to-url (lookup alias-list)
  "Lookup potential alias LOOKUP in ALIAS-LIST, return nil if none."
  (if alias-list
      (if (and (stringp (car alias-list))
               (string= lookup (car alias-list)))
          (if (listp (cadr alias-list))
              (cadr alias-list)
            (list "" (cadr alias-list)))
        (textile-alias-to-url lookup (cddr alias-list)))
    nil))

(defun textile-generate-inline-tag (tag tag-list)
  "Convert textile tag to HTML tag or return nil if no match."
  (if tag-list
      (if (string= tag (car tag-list))
          (cadr tag-list)
        (textile-generate-inline-tag tag (cddr tag-list)))
    nil))

(defun textile-block-clear (my-string)
  "Pass a \"clear:left|right|both\" style to the next block.
The only valid attributes to include in here are \"<\" or \">\" for clearing
left or right floating, or nothing for the default of \"both\"."
  (if (string-match "clear\\([<>]?\\)\\. *" my-string)
      (let ((attrib-string (match-string 1 my-string)))
        (re-search-forward "clear\\([<>]?\\)\\. *" nil t)
        (setq my-string (replace-match "" t nil my-string))
        (list "(FIXME: this needs to be removed by clear code)"
              (plist-put nil 'next-block-clear
                            (cond
                             ((string= attrib-string "<")
                              "clear: left")
                             ((string= attrib-string ">")
                              "clear: right")
                             (t "clear: both")))))
    (textile-error "Clear block's attribute string must be <, >, or nothing.")
    ""))

(defun textile-block-dl (my-string attributes)
  "Handle the definition list block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this definition list."
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended <dl> block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  (if (string-match (textile-this-block-tag-regexp "dl") my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (setq attributes (plist-put attributes 'textile-tag "dl"))
  (let ((my-list (textile-split-string (textile-encode-tags
                                        (textile-encode-escapes my-string))
                                       "\n"))
        (my-new-list nil))
    (dotimes (i (safe-length my-list))
      (let ((this-line (nth i my-list)))
        (if (string-match ":" this-line)
            (setq my-new-list (nconc my-new-list (list this-line)))
          (setcar (nthcdr (- (safe-length my-new-list) 1) my-new-list)
                  (concat (nth (- (safe-length my-new-list) 1) my-new-list)
                          (if textile-br-all-newlines
                              "\n"
                            " ")
                          this-line)))))
    (setq my-list nil)
    (dolist (this-string my-new-list)
      (setq my-list
            (if (string-match "^\\(.*?\\):\\([^\000]*\\)$" this-string)
                (append
                 (list (list (textile-inline-to-list
                              (match-string 2 this-string))
                             (list 'textile-tag "dd"))
                       (list (textile-inline-to-list
                              (match-string 1 this-string))
                             (list 'textile-tag "dt")) "\n") my-list)
              (append (textile-inline-to-list this-string) my-list))))
    (append (reverse (textile-decode-tags
                      (textile-decode-escapes my-list)))
            (list attributes))))

(defun textile-block-list (my-string)
  "Handled the list block MY-STRING with attributes L-ATTRIBUTES."
  (let ((temp-string (substring my-string
                                (length
                                 (plist-get
                                  (textile-attributes "[#*]" my-string)
                                  'textile-attrib-string)))))
    (if (string-match "^\\([#*]+\\)" temp-string)
        (setq temp-string (replace-match "" nil nil temp-string)))
    (if (plist-get (textile-attributes " " temp-string)
                   'textile-well-formed)
        (if (string-match "^\\([#*]+\\)" my-string)
            (let ((my-start-level (length (match-string 1 my-string)))
                  (my-list (textile-split-string my-string "\n")))
              (setq my-list (textile-unsplit-list-newlines my-list))
              (setq my-list (textile-organize-lists my-start-level my-list))
              (textile-nest-lists (textile-process-li my-list))))
      (list (textile-inline-to-list my-string)
            (plist-put
             (plist-put nil 'textile-tag "p")
             'textile-explicit nil)))))

(defun textile-nest-lists (my-list)
  "Nest lists in a way that will produce valid HTML."
  (let ((new-list nil))
    (if (and (listp my-list)
             (= (length my-list) 1))
        (list (textile-nest-lists (car my-list)))
      (dotimes (i (safe-length my-list))
        (let ((this-item (nth i my-list))
              (next-item (if (< i (- (safe-length my-list) 2))
                             (nth (+ i 1) my-list)
                           nil)))
          (if (and (listp this-item) (member 'textile-tag this-item))
              (push this-item new-list)
            (if (not (eq this-item 'dont-push))
                (progn
                  (if (and (textile-get-attributes this-item)
                           (string=
                            (plist-get (textile-get-attributes this-item)
                                       'textile-tag)
                            "li")
                           (textile-get-attributes next-item)
                           (or
                            (string=
                             (plist-get (textile-get-attributes next-item)
                                        'textile-tag)
                             "ol")
                            (string=
                             (plist-get (textile-get-attributes next-item)
                                        'textile-tag)
                             "ul")))
                      (progn
                        (textile-append-block this-item
                                              (textile-nest-lists next-item))
                        (setcar (nthcdr (+ i 1) my-list) 'dont-push)))
                  (push this-item new-list))))))
      (reverse new-list))))

(defun textile-get-attributes (my-list)
  "Return attributes of my-list."
  (if (member 'textile-tag (car (last my-list)))
      (car (last my-list))
    nil))

(defun textile-unsplit-list-newlines (my-list)
  "If any list items don't start with the list tag regexp, join to previous."
  (let ((new-list nil))
    (dolist (this-item my-list)
      (if (string-match textile-list-tag-regexp this-item)
          (push this-item new-list)
        (setcar new-list
                (concat (car new-list)
                        (if textile-br-all-newlines
                            "\n"
                          " ") this-item))))
    (reverse new-list)))

(defun textile-process-li (my-list)
  (let ((first-string-pos 0))
    (while (and (< first-string-pos (safe-length my-list))
                (not (stringp (nth first-string-pos my-list))))
      (setq first-string-pos (1+ first-string-pos)))
    (if (not (< first-string-pos (safe-length my-list)))
        nil
      (if (string-match textile-list-tag-regexp (nth first-string-pos
                                                     my-list))
          (let* ((sample-string (nth first-string-pos my-list))
                 (tag (match-string 2 sample-string))
                 (l-attributes
                  (plist-put
                   (textile-attributes " "
                                       (match-string 1 sample-string))
                   'textile-tag
                   (cond
                    ((string-match "^#" tag)
                     "ol")
                    ((string-match "[*]" tag)
                     "ul")
                    (t "?")))))
            (append (mapcar
                     (function
                      (lambda (x)
                        (if (listp x)
                            (textile-process-li x)
                          (if (string-match "^\\(([^ ]+?)\\|\\)[#*]+" x)
                              (setq x (replace-match "" nil nil x)))
                          (let ((attributes (textile-attributes " " x)))
                            (setq x (substring x (length
                                                  (plist-get
                                                   attributes
                                                   'textile-attrib-string))))
                            (list (textile-inline-to-list x)
                                  (plist-put attributes 'textile-tag
                                             "li"))))))
                     my-list) (list l-attributes)))))))
        
(defun textile-organize-lists (last-level my-list)
  (let ((new-list nil))
    (while my-list
      (let* ((this-item (car my-list))
             (this-level (textile-list-level
                          (if (string-match textile-list-tag-regexp this-item)
                              (match-string 2 this-item))))
             (push-this
              (cond
               ((> this-level last-level)
                (textile-organize-lists this-level my-list))
               ((< this-level last-level)
                nil)
               (t
                this-item)))
             (push-length (if (and push-this (listp push-this))
                              (textile-recursive-length push-this)
                            1)))
        (if push-this
            (push push-this new-list))
        (if (< this-level last-level)
            (setq my-list nil)
          (setq my-list (nthcdr push-length my-list)))))
    (reverse new-list)))

(defun textile-list-level (my-tag)
;  (let ((x (length my-tag)))
;    (if (string-match "^#" my-tag)
;        (+ 1000 x)
;      x)))
  (length my-tag))

(defun textile-recursive-length (my-list)
  (let ((i 0))
    (setq my-list (mapcar (function (lambda (x)
                                      (if (listp x)
                                          (textile-recursive-length x)
                                        1))) my-list))
    (dolist (x my-list)
      (setq i (+ i x)))
    i))

(defun textile-all-but-last (my-list)
  "Return everything in the list but the last item."
  (reverse (cdr (reverse my-list))))

(defun textile-block-table (my-string attributes)
  "Handle the table block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this table."
  (if (string-match (textile-this-block-tag-regexp "table") my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended <table> block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  ; FIXME - BC tables make note of alignment attributes applied to
  ; header lines and then apply those to everything in that row.
  (setq attributes (plist-put attributes 'textile-tag "table"))
  (let ((my-row-list (textile-split-string my-string " *| *\\(?:\n\\|$\\)")))
    (append
     (mapcar 'textile-table-row-process my-row-list) (list attributes))))

(defun textile-table-row-process (this-string)
  (let* ((row-attributes (textile-attributes ". " this-string 'table))
         (this-string (substring this-string
                                 (length
                                  (plist-get row-attributes
                                             'textile-attrib-string))))
         (my-cell-list (textile-split-string this-string " *| *")))
    (setq row-attributes (plist-put row-attributes 'textile-tag "tr"))
    (if (plist-get row-attributes 'textile-header)
        (setq Textile-in-header-row t))
    (prog1
        (append (mapcar 'textile-table-cell-process my-cell-list)
                (list row-attributes))
      (makunbound 'Textile-in-header-row))))

(defun textile-table-cell-process (this-cell)
  (let* ((cell-attributes (textile-attributes "[.] +"
                                              this-cell 'table))
         (header
          (or (boundp 'Textile-in-header-row) ; what an ugly hack
              (plist-get cell-attributes
                         'textile-header))))
    (if (string-match
         (concat "^" (regexp-quote
                      (plist-get cell-attributes
                                 'textile-attrib-string)))
         this-cell)
        (setq this-cell (replace-match "" nil nil
                                       this-cell)))
    (setq cell-attributes (plist-put cell-attributes
                                     'textile-tag
                                     (if header
                                         "th"
                                       "td")))
    (list (textile-inline-to-list this-cell)
          cell-attributes)))

(defun textile-block-p (my-string attributes)
  "Handle the paragraph block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this paragraph."
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended <p> block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  (if (string-match (textile-this-block-tag-regexp "p") my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (setq attributes (plist-put attributes 'textile-tag "p"))
  (list (textile-inline-to-list my-string) attributes))

(defun textile-block-footnote (my-string attributes)
  "Handle the footnote starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this footnote."
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended fn? block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  (if (string-match textile-block-tag-regexp my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (setq attributes (plist-put attributes 'textile-tag "p"))
  (list (list (plist-get attributes 'textile-fn-num)
              (plist-put nil 'textile-tag "sup"))
        " " (textile-inline-to-list my-string) attributes))

(defun textile-block-header (my-string attributes)
  "Handle the header block starting at (point).
Finish at the beginning of the next paragraph, having completely
HTML-formatted this header."
  (when (plist-get attributes 'textile-extended)
    (textile-error "Extended <h?> block doesn't make sense.")
    (setq attributes (plist-put attributes 'textile-extended nil)))
  (let ((my-tag (concat "h" (plist-get attributes 'textile-hlevel))))
    (if (string-match textile-block-tag-regexp my-string)
        (setq my-string (replace-match "" nil nil my-string)))
    (setq attributes (plist-put attributes 'textile-tag my-tag))
    (list (textile-inline-to-list my-string) attributes)))

(defun textile-block-bq (my-string attributes)
  (setq attributes (plist-put attributes 'textile-tag "blockquote"))
  (if (string-match textile-block-tag-regexp my-string)
      (setq my-string (replace-match "" nil nil my-string)))
  (list (list (textile-inline-to-list my-string)
              (plist-put nil 'textile-tag "p"))
        attributes))

(defun textile-error (error-message)
  "Break with an error if textile-error-break is t.
Otherwise just flash the error into the Messages buffer and
continue."
  (if textile-error-break
      (error "%s" error-message)
    (message "Textile error: %s" error-message)))

(defun textile-region (start end)
  "Call textile-code-to-blocks on region from point to mark."
  (interactive "r")
  (save-excursion
    (let ((my-string (buffer-substring start end)))
      (if textile-output-to-new-buffer
          (switch-to-buffer (get-buffer-create
                             (textile-new-buffer-name)))
        (delete-region start end)
        (goto-char start))
      (insert (textile-string my-string)))))

(defun textile-new-buffer-name ()
  "Create new buffer name based on old buffer name."
  (let ((inc 1))
    (if (get-buffer (concat (buffer-name) ".html"))
        (progn
          (while (get-buffer (concat (buffer-name) "-"
                                     (number-to-string inc) ".html"))
            (setq inc (+ inc 1)))
          (concat (buffer-name) "-" (number-to-string inc) ".html"))
      (concat (buffer-name) ".html"))))

(defun textile-buffer ()
  "Call textile-code-to-blocks on the entire buffer."
  (interactive)
  (textile-region (point-min) (point-max)))

(defun textile-string (my-string)
  "Process MY-STRING, return XHTML-encoded string."
  (let ((current-case-fold-search case-fold-search))
    (setq case-fold-search nil)
    (setq textile-xhtml-version textile-xhtml-version-default)
    (setq Textile-escapes nil)
    (setq Textile-tags nil)
    (setq Textile-manual nil)
    (prog1
        (textile-list-to-blocks (textile-string-to-list my-string))
      (setq case-fold-search current-case-fold-search))))

(defun textile-generate-attribute-string (my-plist)
  "Generate an attribute string based on MY-PLIST.
Any attributes that start with \"textile-\" will be ignored."
  (if (and (car my-plist) (cadr my-plist))
      (let* ((attrib (format "%s" (car my-plist)))
             (value
              (if (stringp (cadr my-plist))
                  (format "%s" (cadr my-plist))
                (if (string= attrib "style")
                    (mapconcat 'identity (cadr my-plist) "; ")
                  (if (string= attrib "class")
                      (mapconcat 'identity (cadr my-plist) " ")
                    (format "%s" (cadr my-plist)))))))
        (if (string-match "^textile-" attrib)
            (textile-generate-attribute-string (cddr my-plist))
          (concat " " attrib "=\"" value "\""
                  (textile-generate-attribute-string (cddr my-plist)))))
    (if (cddr my-plist)
        (textile-generate-attribute-string (cddr my-plist))
      "")))

(defun textile-enclose-tag (my-string my-plist)
  "Enclose MY-STRING in a tag generated from information in MY-PLIST."
  (let ((my-tag (plist-get my-plist 'textile-tag)))
    (if my-tag
      (cond
       ((string= my-tag "img")
        (concat "<img" (textile-generate-attribute-string my-plist)
                " />"))
       (t
        (concat "<" (plist-get my-plist 'textile-tag)
                (textile-generate-attribute-string my-plist) ">"
                my-string "</" (plist-get my-plist 'textile-tag) ">")))
      my-string)))

; functions to support batch mode (for CGI operation, perhaps)

(defun textile-batch-read ()
  "Read file from STDIN."
  (read-from-minibuffer ""))

(defun textile-batch-write (arg)
  "Write output to STDOUT."
  (send-string-to-terminal arg))

(when noninteractive
  (let ((input-string ""))
    (while (condition-case nil
	       (setq current-line (textile-batch-read))
	     (error nil))
      (while (string-match "\r" current-line)
	(setq current-line (replace-match "" nil nil current-line)))
      (setq input-string (concat input-string current-line "\n")))
    (textile-batch-write (textile-string
			  (substring input-string 0
				     (- (length input-string) 1))))))

(provide 'textile)