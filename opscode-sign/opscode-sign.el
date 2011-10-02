;; opscode-sign.el -- Opscode API request signer

;; Version:  0.1
;; Keywords: elisp, RSA, openssl, signature, opscode, chef
;; Date:     2011-05-14
;; Author:  Christopher Brown (skeptomai) <cb@opscode.com>
;; Maintainer: Christopher Brown (skeptomai) <cb@opscode.com>

;; License:: Apache License, Version 2.0
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;     http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;;; Commentary:
;;
;; Relies on the OpenSSL command line tool to generate an RSA signature 
;; for a request

;;; Code:

(require 'cl)
(require 'sha1)
(require 'base64)

(defvar *SIGNING_DESCRIPTION* "version=1.0")
(defvar *CHEF_VERSION* "0.10.2")

(defcustom opscode-userid nil
  "User ID for Opscode signed requests"
  :type '(string)
  :group 'http-twiddle)

(defcustom opscode-userpem nil
  "Filename for user's pem to sign Opscode requests"
  :type '(string)
  :group 'http-twiddle)

(defun opscode-request-body (request-buffer-string)
  (let* ((rx (concat "\\(.*\\(?:\n.*\\)*?\\)"   ;; definition: to end of line,
                                                ;; then maybe more lines
                                                ;; (excludes any trailing \n)
                     "\\(?:\n\\s-*\n\\|\\'\\)"  ;; blank line or EOF
                     "\\(.*\\)"))
         (matched (string-match rx request-buffer-string) ) 
         (path-and-headers (match-string 1 request-buffer-string) )
         (body (or (match-string 2 request-buffer-string) "") ))
    (values path-and-headers body)))

(defun opscode-sign-buffer-request (request-buffer-string)
  (multiple-value-bind (path-and-headers body) (opscode-request-body request-buffer-string)
    (multiple-value-bind (http-method path) (split-string path-and-headers)
      (let ((signed-header-block (opscode-header-block 
                                  (opscode-sign-request 
                                   http-method 
                                   path
                                   body
                                   (opscode-canonicalize-time)
                                   opscode-userid
                                   opscode-userpem))))
        (wipe-buffer)
        (if (> (length body) 0 )
            (insert (format "%s\n%s: %d\n%s\n\n%s\n" path-and-headers "Content-Length" (length body) signed-header-block 
                            body) )
          (insert (format "%s\n%s\n\n" 
                          path-and-headers
                          signed-header-block)))) )))

(defun wipe-buffer ()
      (let ((inhibit-read-only t))
        (widen)
        (delete-region (point-min) (point-max))))

(defun opscode-canonicalize-time ()
   (format-time-string "%Y-%m-%dT%TZ")  )

(defun opscode-canonicalize-path (path)
  "Replace repeated slashes and trailing slash from path."
  (let ((reduced-string  (replace-regexp-in-string "\/+" "/" path) ))
    (replace-regexp-in-string "\/$" "" reduced-string)))

(defun opscode-hash-content (content-string)
  (interactive "MContent to hash: \n")
  (chomp (base64-encode-string (sha1-binary content-string))))

(defun opscode-canonicalize-request (http-method canonical-path hashed-body canonical-time user-id)
  (interactive "MMethod: \nMCanonical Path: \nMHashed-body: \nMCanonical Time: \nMUser Id: \n")
  (format "Method:%s\nHashed Path:%s\nX-Ops-Content-Hash:%s\nX-Ops-Timestamp:%s\nX-Ops-UserId:%s"
          (upcase http-method) (opscode-hash-content canonical-path) hashed-body canonical-time user-id))

(defun opscode-sign-content (private-key-file content-string)
  (interactive "fPrivate Key: \nMContent: \n")
  (shell-command-to-string (format "echo -n \"%s\" | openssl rsautl -sign -inkey %s" content-string private-key-file) ))

(defun opscode-sign-request (http-method path body canonical-time user-id private-key)
  (let* ((canonical-path (opscode-canonicalize-path path) )
         (hashed-body (opscode-hash-content body) )
         (signature (chomp
                    (base64-encode-string 
                     (opscode-sign-content private-key 
                                   (opscode-canonicalize-request 
                                    http-method 
                                    canonical-path 
                                    hashed-body 
                                    canonical-time user-id))))))
    `(("X-Ops-Authorization-1" . ,signature)
      ("X-Ops-Sign" . ,*SIGNING_DESCRIPTION*)
      ("X-Chef-Version" . ,*CHEF_VERSION*)
      ("X-Ops-Userid" . ,user-id)
      ("X-Ops-Timestamp" . ,canonical-time)
      ("X-Ops-Content-Hash" . ,hashed-body))))

(defun opscode-header-block (header-list) (mapconcat (lambda (item) (format "%s: %s" (car item) (cdr item))  ) header-list "\n") ) 

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

;; Test


(provide 'opscode-sign)
