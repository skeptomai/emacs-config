;; Get nick and nickserv identify info
(require 'cl)

(let ((ercinfo (concat *home-path* "/.ercinfo")))
  (if (file-exists-p ercinfo)
       (load-file ercinfo)))

;; ERC mode stuff
(autoload 'erc "erc" "IRC in Emacs" t nil)
(autoload 'erc-autojoin-mode "erc-join" "ERC autojoin" t nil)
                              
(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (progn
      (add-hook 'erc-after-connect
                '(lambda (SERVER NICK)
                  (cond
                    ((string-match "freenode\\.net" SERVER)
                     (erc-message "PRIVMSG" (concat "Nickserv identify " erc-nick-id))))))
      
      (erc-autojoin-mode 1)
      
      (erc :server erc-server :port erc-port
           :nick erc-nick :full-name erc-user-full-name))))

;;; Notify me when a keyword is matched (someone wants to reach me)

(defvar my-erc-page-message "-m %s says \"%s\""
  "Format of message to display in dialog box")

(defvar my-erc-page-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defvar my-erc-page-timeout 30
  "Number of seconds that must elapse between notifications from
the same person.")

(defun my-erc-page-popup-notification (nick message)
  (when window-system
    ;; must set default directory, otherwise start-process is unhappy
    ;; when this is something remote or nonexistent
    (let ((default-directory *home-path*))
      (call-process "/usr/local/bin/growlnotify" nil nil nil "IRC message" 
                    (format my-erc-page-message nick message)))))

(defun my-erc-page-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`my-erc-page-timeout'."
  (unless delay (setq delay my-erc-page-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick my-erc-page-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
        (push (cons nick cur-time) my-erc-page-nick-alist)
        t)))

(defun my-erc-page-me (match-type nick message)
  "Notify the current user when someone sends a message that
matches a regexp in `erc-keywords'."
  (interactive)
  (when (and (eq match-type 'keyword)
             ;; I don't want to see anything from the erc server
             (null (string-match "\\`\\([sS]erver\\|localhost\\)" nick))
             ;; or bots
             (null (string-match "\\(bot\\|serv\\)!" nick))
             ;; or from those who abuse the system
             (my-erc-page-allowed nick))
    (my-erc-page-popup-notification nick message)))

(add-hook 'erc-text-matched-hook 'my-erc-page-me)

(defun my-erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (my-erc-page-allowed nick))
      (my-erc-page-popup-notification nick msg)
      nil)))

(add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG)

