;; Remember integration
(require 'remember)
;;(require 'remember-autoloads)


(setq org-mobile-directory "~/Dropbox/MobileOrg")

(setq org-export-latex-listings 'minted)
(setq org-export-latex-custom-lang-environments
      '((emacs-lisp "common-lispcode")))
(setq org-export-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
        ("linenos" "")))
(setq org-latex-to-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; typically don't need sub/super, but I do have underscores in variable names [cb]
(setq org-export-with-sub-superscripts nil)
;; Don't include selected text in the link
(setq org-context-in-file-links nil)

(setq org-enforce-todo-dependencies t)
;; completion in org
(setq org-completion-use-ido t)

(setq org-log-done 'time)
(setq org-log-done 'note)

(setq org-remember-templates
      '(("AppleScript remember" ?y "* %:shortdesc\n  %:initial\n   Source: %u, %c\n\n  %?" (concat org-directory "inbox.org") "Remember")
        ("AppleScript note" ?z "* %?\n\n  Date: %u\n" (concat org-directory "inbox.org") "Notes")
        ("Code" ?c "* %?\n %i\n %A" "~/org/code.org" "Code")
        ("Tasks" ?t "* TODO %?\n  %i\n  %a" "~/org/organizer.org")
        ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a" "~/org/organizer.org"))
      )
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))

(eval-after-load 'remember
  '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember)       

;; Org integration
(require 'org)
(autoload 'org-mode "org-mode" "org mode" t nil)
(setq auto-mode-alist
      (append '(("\\.org$" . org-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("TODO$" . org-mode)) auto-mode-alist))
(global-set-key (kbd "C-c a") 'org-agenda)                                       
(setq org-todo-keywords '("TODO" "NOTE" "STARTED" "WAITING" "DONE"))
(setq org-agenda-include-diary t)                                             
(setq org-agenda-include-all-todo t)
(setq org-tag-alist '(("EPIC") ("BILLING") ("CHEF") ("CI") ("CORPSITE") ("OPERATIONS") 
                      ("PLATFORM") ("RACKSPACE") ("REPORTING") ("ROOT_CAUSE") 
                      ("SEARCH") ("AUTHZ")))

(require 'find-lisp)
(let ((pbis "~/Projects/opscode/main/pbime/pbi-dev/pbis"))
  (when (file-accessible-directory-p pbis)
    (setq org-agenda-files (find-lisp-find-files pbis "\\.org$") )
    (require 'org-publish)
    (setq org-publish-project-alist
          '(
            ("pbis"
             :base-directory "~/Projects/opscode/main/pbime/pbi-dev/pbis"
             :base-extension "org"
             :publishing-directory "~/Sites/"
             :recursive t
             :publishing-function org-publish-org-to-html
             :headline-levels 4             ; Just the default for this project.
             :auto-preamble t
             )
            ))
    ))

(setq-default org-startup-indented t)

;; (load-library "../org-contrib/org-export-generic.el")

(when (functionp 'org-set-generic-type)
  (org-set-generic-type
   "confluence"
   '(
     :file-suffix ".txt"
                  :key-binding                   ?c

                  :header-prefix            	    "headerprefix"
                  :header-suffix            	    "headersuffix"

                  :title-format             	    "h1. %s\n"

                  :date-export        	    nil

                  :toc-export                    nil

                  :body-header-section-numbers   nil
                  :body-section-prefix           "\n"

                  :body-section-header-prefix    ("h2. " "h3. " "h4. "
                                                  "h5. " "h6. ")

                  :body-section-header-suffix    ("\n" "\n" "\n"
                                                  "\n" "\n")

                  :body-line-export-preformated  t          ;; yes/no/maybe???
                  :body-line-format              "%s\n"
                  :body-line-wrap                75

                  :body-line-fixed-format       " %s\n"

                  :body-list-format              "* %s\n"
                  :body-number-list-format       "# %s\n"

                  :body-bullet-list-prefix       ("* " "** " "*** " "**** " "***** ")
                  )))
