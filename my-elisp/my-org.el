;; Remember integration
(require 'remember)
(require 'remember-autoloads)

(setq org-enforce-todo-dependencies t)
;; completion in org
(setq org-completion-use-ido t)

(setq org-log-done 'time)
(setq org-log-done 'note)

(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n  %i\n  %a" "~/org/organizer.org")
        ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a" "~/org/organizer.org")))
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
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "DONE"))                 
(setq org-agenda-include-diary t)                                             
(setq org-agenda-include-all-todo t)
(setq org-tag-alist '(("EPIC") ("BILLING") ("CHEF") ("CI") ("CORPSITE") ("OPERATIONS") ("PLATFORM") ("RACKSPACE") ("REPORTING") ("ROOT_CAUSE") ("SEARCH") ("AUTHZ")))

(require 'find-lisp)
(setq org-agenda-files (find-lisp-find-files "~/Projects/opscode/main/pbime/pbis" "\\.org$") )


