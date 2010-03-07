;;
;; Global key settings
;; This feels wrong and artificial, because it cuts across all modes and libraries
;; which may or may not be installed.  Nonetheless, it makes finding all those
;; weird bindings easier when debugging [skeptomai]
;;

(global-set-key "\M-," 'point-to-top)
(global-set-key "\C-x," 'tags-loop-continue)
(global-set-key "\M-." 'point-to-bottom)
(global-set-key "\C-x\C-m" 'mark-defun)
(global-set-key "\C-cm" 'execute-extended-command)
(global-set-key "\C-cs" 'save-buffer)
(global-set-key "\C-cb" 'end-of-buffer)
(global-set-key "\C-ct" 'beginning-of-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ck" 'kill-buffer)
(global-set-key "\C-cc" 'save-buffers-kill-emacs)
(global-set-key "\C-cf" 'ido-find-file)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-ce" 'fc-eval-and-replace)
(global-set-key (quote [f12]) 'slime-selector)
(global-set-key "\C-cd" 'toggle-selective-display)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\M-r" 'isearch-backward-regexp)
(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\C-cl" 'cltl)
(global-set-key "\C-c\C-xr" 'run-ruby)
(global-set-key "\M-g" 'grep-find)
(global-set-key "\C-q" 'scroll-n-lines-behind)
(global-set-key "\C-z" 'scroll-n-lines-ahead)
(global-set-key "\C-x\C-q" 'quoted-insert)
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key "\C-x\C-p" 'other-window-backward)
(global-set-key "\C-x\C-n" 'other-window)
(global-set-key (quote [f6]) 'toggle-writeroom)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)