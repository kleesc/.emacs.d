;;; init.el --- 
;;
;;; Commentary:
;;
;; Author: Kenny Lee Sin Cheong
;; URL: https://kleesc.net
;;
;; TODO: Seperate into smaller manageable modules
;;
;;; Code:

;; ==================
;;  CUSTOM FUNCTIONS
;; ==================
;; 1.2 Multi-buffer search
(defun search (regexp)
  "Search all buffers for a REGEXP."
  (interactive "sRegexp to search for: ")
  (multi-occur-in-matching-buffers ".*" regexp))

;; Cyberpunk Cursor
(defvar blink-cursor-colors (list  "#92c48f" "#6785c5" "#be369c" "#d9ca65")
  "On each blink the cursor will cycle to the next color in this list.")

(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Cyberpunk variant of timer 'blink-cursor-timer'.  
OVERWRITES original version in 'frame.el'.
This one changes the cursor color on each blink.
Define colors in 'blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
        (when (>= blink-cursor-count (length blink-cursor-colors))
              (setq blink-cursor-count 0))
        (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
        (setq blink-cursor-count (+ 1 blink-cursor-count)))
  (internal-show-cursor nil (not (internal-show-cursor-p))))

;; Fullscreen toggle
(defun toggle-fullscreen ()
  "Toggle full screen on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))


;; ======
;;  Gnus
;; ======
(setq gnus-home-directory "~/.emacs.d/Gnus/")

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
;;;;(add-to-list 'gnus-secondary-select-methods
;;;;             '(nnimap "gmail"
;;;;                      (nnimap-address "imap.gmail.com")
;;;;                      (nnimap-server-port 993)
;;;;                      (nnimap-authinfo-file "~/.authinfo.gpg")
;;;;                      (nnimap-stream ssl)))

;; Smtp gmail
;;;;(require 'starttls)
;;;;(require 'smtpmail)
;;;;(setq message-send-mail-function 'smtpmail-send-it
;;;;      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;;;      smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;;;                                   "kenny.lee28@gmail.com" nil))
;;;;      smtpmail-default-smtp-server "smtp.gmail.com"
;;;;      smtpmail-smtp-server "smtp.gmail.com"
;;;;      smtpmail-smtp-service 587
;;;;      gnus-ignored-newsgroups "^to\\.\\[^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

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
(load "~/.emacs.d/.erc-auth")

(add-hook 'erc-after-connect
          '(lambda (SERVER NICK)
             (cond
              ((string-match "freenode\\.net" SERVER)
               (erc-message "PRIVMSG" (concat "Nickserv identify " irc-freenode-nick-passwd))))))

;; Autojoin
(require 'erc-join)
(erc-autojoin-mode 1)
;;(setq erc-autojoin-timing ident)
;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#ruby" "#python-dev")))

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

;; Finally, connect to the networks.
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
;; ------------
;; Key bindings
;; ------------
;; (global-set-key (kbd "<f5>") 'copy-region-as-kill)
(global-set-key (kbd "<prior>") (lambda () (interactive) (scroll-down 3)))
(global-set-key (kbd "<next>")  (lambda () (interactive) (scroll-up 3)))
(global-set-key (kbd "C-x C-b") 'ibuffer) ;; list-buffer replacement
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<f8>") 'window-configuration-to-register)
(global-set-key (kbd "<f9>") 'jump-to-register)

;; GUI
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Modeline information
(display-time-mode t)
(column-number-mode t)
(display-battery-mode t)

;; Minibuffer handling (Ido Mode)
(ido-mode t)
(setq ido-enable-flex-matching t)
(ido-everywhere)
;; Because using arrow keys is annoying
(define-key ido-file-completion-map (kbd "C-p") 'ido-prev-match)
(define-key ido-file-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-file-dir-completion-map (kbd "C-p") 'ido-prev-match)
(define-key ido-file-dir-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-buffer-completion-map (kbd "C-p") 'ido-prev-match)
(define-key ido-buffer-completion-map (kbd "C-n") 'ido-next-match)

;; recentf Setup
(recentf-mode t)
(setq recentf-auto-cleanup 'never)

;; Startup
(setq inhibit-startup-screen 1)
(setq initial-scratch-message "")

;; Prevent backups from littering the file system
(setq backup-directory-alist '(("." . "~/emacsbackups")))

;; Parentheses
;;(electric-pair-mode t)
(show-paren-mode t)

;; Tabs handling
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Scrolling
(setq scroll-conservatively 10)
(setq scroll-margin 2) ;;7

;; Line wrapping
(global-visual-line-mode 1)

;; End of file newlines
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; Region selection
(transient-mark-mode 0)

;; Clipboard behaviour
(setq-default x-select-enable-clipboard t)

;; Syntax highlighting (font locking)
(global-font-lock-mode t)

;; Javascript-mode tab length
(setq js-indent-level 2)

;; Mouse avoidance
;; (mouse-avoidance-mode 'banish)

;; Browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome")

;; Emacs server
;;(server-start) ---------BUGS IN WINDOWS

;; ANSI Color in terminals
(ansi-color-for-comint-mode-on)

;; Advanced commands
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Font size (in 24, font is unusually bigger...)
(set-face-attribute 'default nil :height 120) ;; In 1/10 pt: 100*0.10 = 10pt


;; ==============
;;  Applications
;; ==============
;; ---------------------
;; 3rd party elisp files
;; ---------------------
(add-to-list 'load-path "~/.emacs.d/packages/")
;; Little Man Computer Simulator (Stefan Monnier IFT1215)
(require 'lmc)
;; Gambit Scheme (Mark Feeley IFT2035)
(require 'gambit)

;; ------------
;; Repositories
;; ------------
(require 'package)
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Not always up to date repos:
;; ("marmalade" . "http://marmalade-repo.org/packages/")
;; ("gnu" . "http://elpa.gnu.org/packages/") ;; Included

;; Initialize packages and activate them before the end of the script.
;; (Usually done after the script is ran, so package initialization can be tricky)
(setq package-enable-at-startup nil)
(package-initialize)

;; ----------------
;; Package settings
;; ----------------
;; Theme
(load-theme 'junio t)

;; Gimmicky Nyan cat (not practical on smaller screens)
;; (case window-system
;;   ((x w32) (nyan-mode)))

;; HTML & CSS hex color
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode t)
(add-hook 'term-mode-hook
          (lambda() ;; Yas interferes with tab completion in ansi-term.
            (setq yas-dont-activate t)))
;;(yas/initialize)
;;(yas/load-directory "~/.emacs.d/packages/yasnippet-master/snippets")
;;(setq yas/prompt-functions '(yas/dropdown-prompt)) ;; Uses dropdown-list.el instead of OS window

;; Auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;;;;;(add-hook 'js2-mode-hook 'ac-js2-mode)
;;(ac-set-trigger-key "TAB")
;;(ac-set-trigger-key "<tab>")
;;(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)
;;(setq ac-trigger-key "C-i")

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; Flycheck tip
(require 'flycheck-tip)
(flycheck-tip-use-timer 'verbose)

;; Magit
(require 'magit)

;; Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; js2-mode 
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode) ;; Linting
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; For more comprehensive completions you can opt to evaluate the code
;; for candidates. A browser needs to be connected to Emacs for the
;; evaluation completions to work. Put this in your init.el file.
;; `(setq ac-js2-evaluate-calls t)'

;; To add completions for external libraries add something like this:
;; (add-to-list 'ac-js2-external-libraries "path/to/lib/library.js")

;; Then connect a browser to Emacs by calling `(run-skewer)'. You may
;; need to save the buffer for completions to start.

;; If auto-complete mode is installed on your system then completions
;; should start showing up otherwise use `completion-at-point'.

;; Note: library completions will only work if `ac-js2-evaluate-calls'
;; is set and a browser is connected to Emacs.


;; Web-mode (When editing script in markup files) DOCS: http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; 2 Space indent
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Go mode
(require 'go-mode)
(add-hook 'before-save-hook #'gofmt-before-save)

;; Python
;; (setq python-shell-virtualenv-path "")

;; Haskell mode
(require 'haskell-mode)

;; Yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Helm Stuff
;; ----------
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; list actions using C-z
(define-key helm-map (kbd "C-z")  'helm-select-action)

(when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

;; 1. open helm buffer inside current window, not occupy whole other window
;; 2. move to end or beginning of source when reaching top or bottom of source.
;; 3. search for library in `require' and `declare-function' sexp.
;; 4. scroll 8 lines other window using M-<next>/M-<prior>
;; 5. use recentf
(setq helm-split-window-in-side-p           t 
      helm-move-to-line-cycle-in-source     t 
      helm-ff-search-library-in-sexp        t 
      helm-scroll-amount                    8 
      helm-ff-file-name-history-use-recentf t)

(helm-autoresize-mode t)

;; Fuzzy matching
(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match t)

(defun toggle-helm-mode-on ()
  "Toggle helm-mode on."
  (interactive)
  (progn (global-set-key (kbd "M-x") 'helm-M-x)
         (global-set-key (kbd "M-y") 'helm-show-kill-ring)
         (global-set-key (kbd "C-x b") 'helm-mini)
         ;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
         (helm-mode t)))

(defun toggle-helm-mode-off ()
  "Toggle helm-mode off."
  (interactive)
  (progn (global-set-key (kbd "M-x") 'execute-extended-command)
         (global-set-key (kbd "M-y") 'yank-pop)
         (global-set-key (kbd "C-x b") 'ido-switch-buffer)
         ;;(global-set-key (kbd "C-x C-f") 'ido-find-file)
         (helm-mode 0)))

(defun toggle-helm-mode ()
  "Toggle helm-mode on/off."
  (interactive)
  (if helm-mode (toggle-helm-mode-off)
      (toggle-helm-mode-on)))
(global-set-key (kbd "<f12>") 'toggle-helm-mode)

;; Helm-gtags
;; ----------
(setq helm-gtags-ignore-case t
      helm-gtags-auto-update t
      helm-gtags-use-input-at-cursor t
      helm-gtags-pulse-at-cursor t
      helm-gtags-prefix-key "\C-cg"
      helm-gtags-suggested-key-mapping t)

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;;; init.el ends here
