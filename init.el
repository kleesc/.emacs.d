;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; ==================
;;  CUSTOM FUNCTIONS
;; ==================
;; 1.2 Multi-buffer search
(defun search (regexp)
  "Search all buffers for a regexp."
  (interactive "sRegexp to search for: ")
  (multi-occur-in-matching-buffers ".*" regexp))

;; 1.4 Cyberpunk Cursor
(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")

(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Cyberpunk variant of timer 'blink-cursor-timer'. OVERWRITES original version in 'frame.el'.

This one changes the cursor color on each blink. Define colors in 'blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
        (when (>= blink-cursor-count (length blink-cursor-colors))
              (setq blink-cursor-count 0))
        (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
        (setq blink-cursor-count (+ 1 blink-cursor-count))
        )
  (internal-show-cursor nil (not (internal-show-cursor-p)))
  )


;; ===========
;;  LOAD PATH
;; ===========

;; ======
;;  Gnus
;; ======
(setq gnus-select-method '(nnnil))
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq w3m-goto-article-function 'browse-url)
;(define-key gnus-article-mode-map [?M] 'browse-url-firefox)
;w3m-view-url-with-external-browser


;; append "pinentry-program /usr/bin/pinentry-curses" to
;; /home/ken/.gnupg/gpg-agent.conf
;; to use non-graphical identification
;;
;; Remove user agent with (J r) from gmail from server buffer (^)
;; (A z) for zombie groups, (A k) for killed groups
;; (S k) to kill group
;; (B m) to move article

(setq user-mail-address "kenny.lee28@gmail.com")
(setq user-full-name "Kenny Lee Sin Cheong")


(require 'gnus) ;; Solve the "Symbol's value as variable is void" error


;; Gmane
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))
(setq gnus-newsgroup-maximum-articles 10000)


;; Imap gmail
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-authinfo-file "~/.authinfo.gpg")
                      (nnimap-stream ssl)))

;; Smtp gmail
;(require 'starttls)
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                   "kenny.lee28@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\[^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; all mails should be always displayed in the mailbox
;(setq gnus-permanently-visible-groups ".*INBOX")

;; Message citation
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:")

;; Message signature
(setq gnus-posting-styles
      '((".*"
         (signature "Kenny Lee Sin Cheong"))))

;; Contact autocompletion with Bbdb
(setq bbdb/news-auto-create-p t)

;; Three panes layout
(gnus-add-configuration
 '(article
   (horizontal 1.0
               (vertical 25
                         (group 1.0))
               (vertical 1.0
                         (summary 0.25 point)
                         (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
               (vertical 25
                         (group 1.0))
               (vertical 1.0
                         (summary 1.0 point)))))

;; Two panes layout
;; (gnus-add-configuration
;;  '(article (vertical 1.0 (summary .35 point) (article 1.0))))

;; Unicode Formatting
(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date)
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")

;; Start gnus with topics toggled
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Group Formatting (topics)
(setq gnus-topic-line-format "%i[ %u&topic-line; ] %v\n")

;; this corresponds to a topic line format of "%n %A"
(defun gnus-user-format-function-topic-line (dummy)
  (let ((topic-face (if (zerop total-number-of-articles)
                        'my-gnus-topic-empty-face
                        'my-gnus-topic-face)))
    (propertize
     (format "%s %d" name total-number-of-articles)
     'face topic-face)))

;; (T n) to add topic, (T m) to move .
;; (gnus-topic-unindent) M-TAB doesn't work... System switch windows(Alt-TAB)

;; =====
;;  ERC
;; =====
(add-hook 'erc-after-connect
          '(lambda (SERVER NICK)
             (cond
              ((string-match "freenode\\.net" SERVER)
               (erc-message "PRIVMSG" "Nickserv identify ew231966"))

              ((string-match "rizon\\.net" SERVER)
               (erc-message "PRIVMSG" "Nickserv identify ew231966")))))


;; Autojoin
(require 'erc-join)
(erc-autojoin-mode 1)
;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#ruby" "#python-dev")
;;         ("rizon.net" "#bakabt")))


(require 'erc-match)
(setq erc-keywords '("kleesc"))
(erc-match-mode)

(require 'erc-track)
(erc-track-mode t) ; was (erc-track-modified-channels-mode t)
                   ; Note: erc-track-modified-channels-mode changed
                   ; to erc-track-mode as of erc-track.el
                   ; CVS revision 1.23 (November 2002)

(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))


(require 'erc-fill)
(erc-fill-mode t)

(require 'erc-ring)
(erc-ring-mode t)

(require 'erc-netsplit)
(erc-netsplit-mode t)

(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(erc-button-mode nil) ;slow

;(setq erc-user-full-name "Kenny Lee Sin Cheong")
;(setq erc-email-userid "")

;; logging
;;(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/.irclogs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)

;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;   (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
;;                                              (not (null buffer-file-name)))))))

;; (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
;; (add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
;;                                            (set (make-variable-buffer-local
;;                                                  'coding-system-for-write)
;;                                                 'emacs-mule))))
;; end logging
;; something wrong when trying to close emacs using windows [X] button
;; probably (save-buffers-kill-emacs) related

;; Truncate buffers so they don't hog core.
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)


;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)


;;; Finally, connect to the networks.
(defun conn-erc ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
        (erc :server "irc.freenode.net" :port 6667 
             :nick "kleesc" :full-name "Kenny Lee Sin Cheong")
        ;(erc :server "irc.rizon.net" :port 6667 :nick "kleesc")
        ))



;; ==================
;;  EDITOR BEHAVIOUR
;; ==================
;; 3.1 Prevent backups from littering the file system
(setq backup-directory-alist '(("." . "~/emacsbackups")))

;; Font size (in 24, font is unusually bigger...)
;(set-face-attribute 'default nil :height 100) ;; In 1/10 pt 100*10 = 10pt

;; Region selection
(transient-mark-mode 0)

;; Clipboard behaviour
(setq-default x-select-enable-clipboard t)

;; Syntax highlighting (font locking)
(global-font-lock-mode t)

;; Tabs handling
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Javascript-mode tab length
(setq js-indent-level 2)

;; Minibuffer handling
(ido-mode t)

;; Modeline information
(display-time-mode t)
(column-number-mode t)
(display-battery-mode t)

;; Scrolling
(setq scroll-conservatively 10)
(setq scroll-margin 2) ;;7
(setq inhibit-startup-screen 1)
(global-set-key (kbd "<prior>")    (lambda () (interactive) (scroll-down 3)))
(global-set-key (kbd "<next>")  (lambda () (interactive) (scroll-up 3)))

;; Parentheses
(show-paren-mode t)

;; Line wrapping
(global-visual-line-mode 1)

;; End of file newlines
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; Browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome")

;; Emacs server
;;(server-start) ---------BUGS IN WINDOWS

;; ------------
;; Key bindings
;; ------------
;; (global-set-key [f5] 'copy-region-as-kill)


;; ==============
;;  GUI Settings
;; ==============
(tool-bar-mode -1)
;;(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Color themes
(add-to-list 'load-path "~/.emacs.d/packages/color-theme-6.6.0")
(require 'color-theme)
(color-theme-initialize)
(color-theme-comidia)


;; ==============
;;  Applications
;; ==============
;; ---------
;; Yasnippet
;; ---------
(add-to-list 'load-path "~/.emacs.d/packages/yasnippet-master")
(require 'yasnippet) ;; Not yasnippet-bundle

;;(yas/initialize)
;;(yas/load-directory "~/.emacs.d/packages/yasnippet-master/snippets")
(yas/global-mode 1)

(setq yas/prompt-functions '(yas/dropdown-prompt)) ;; Uses dropdown-list.el instead of OS window

;; ----------
;; Zencodding
;; ----------
(add-to-list 'load-path "~/.emacs.d/packages/rooney-zencoding-fc15836")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; -----
;; Magit
;; -----
(add-to-list 'load-path "~/.emacs.d/packages/magit-1.2.0")
(require 'magit)

;; ------------------------------------------------------
;; Little Man Computer Simulator (Stefan Monnier IFT1215)
;; ------------------------------------------------------
(load "~/.emacs.d/packages/lmc")

;; -----------------------------------
;; Gambit Scheme (Mark Feeley IFT2035)
;; -----------------------------------
(load "~/.emacs.d/packages/gambit")

;; -------------
;; Auto-complete
;; -------------
(add-to-list 'load-path "~/.emacs.d/packages/auto-complete-1.3.1")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete-1.3.1/dict")
(ac-config-default)

;; =======
;;  Modes
;; =======
;; ANSI Color in terminals
(ansi-color-for-comint-mode-on)

;; recentf Setup
(recentf-mode t)
(setq recentf-auto-cleanup 'never)

;; Advanced commands
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")


;; 3.22 Mouse avoidance
;; (mouse-avoidance-mode 'banish)

;; 3.23 Electric Pair
;; (electric-pair-mode t)



