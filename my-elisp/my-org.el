;; Remember integration
(require 'remember)
(require 'remember-autoloads)

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
      '(("Code" ?c "* %?\n %i\n %A" "~/org/code.org" "Code")
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
