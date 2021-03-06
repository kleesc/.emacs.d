;;; init.el --- 
;;
;;; Commentary:
;;
;; Author: Kenny Lee Sin Cheong
;; URL: https://kleesc.com
;;
;;
;;; Code:

;; ==================
;;  CUSTOM FUNCTIONS
;; ==================
(define-globalized-minor-mode 
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
  (text-scale-set 1)
  (kill-local-variable 'text-scale-mode-amount)
  (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
  (global-text-scale-mode 1)
  )

(global-set-key (kbd "M-0")
		'(lambda () (interactive)
			 (global-text-scale-adjust (- text-scale-mode-amount))
			 (global-text-scale-mode -1)))
(global-set-key (kbd "M-+")
		'(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--")
		'(lambda () (interactive) (global-text-scale-adjust -1)))



;; Multi-buffer search
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

;; Sync scratch buffer with file
(defvar scratch-buffer-file-name "~/Dropbox/.scratch" "Scratch buffer.")
(defun sync-scratch-with-file ()
  "Replace *scratch* buffer with scratch-buffer-file-name."
  (save-window-excursion
   (find-file scratch-buffer-file-name)
   (kill-buffer "*scratch*")
   (rename-buffer "*scratch*")
   (org-mode)))
(when (file-exists-p scratch-buffer-file-name) (sync-scratch-with-file))

;; ======
;;  Gnus
;; ======
(setq gnus-home-directory "~/.emacs.d/Gnus/")
(setq user-mail-address "kenny.lee28@gmail.com")
(setq user-full-name "Kenny Lee Sin Cheong")

(setq gnus-select-method '(nnnil))
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq w3m-goto-article-function 'browse-url)
;(define-key gnus-article-mode-map [?M] 'browse-url-firefox)
;w3m-view-url-with-external-browser

(require 'gnus) ;; Solve the "Symbol's value as variable is void" error

;; Gmane
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))
(setq gnus-newsgroup-maximum-articles 2000)

;; Imap gmail
(add-to-list 'gnus-secondary-select-methods
            '(nnimap "gmail"
                     (nnimap-address "imap.gmail.com")
                     (nnimap-server-port 993)
                     (nnimap-authinfo-file "~/.authinfo.gpg")
                     (nnimap-stream ssl)))

;; Smtp gmail
(require 'starttls)
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials "~/.authinfo.gpg"
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
         (signature "Kenny Lee Sin Cheong")
	 ("Cc" ""))))

;; Unicode Formatting
(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
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

;; (gnus-topic-unindent) M-TAB doesn't work... System switch windows(Alt-TAB)

;; Three panes layout
;; (gnus-add-configuration
;;  '(article
;;    (horizontal 1.0
;;                (vertical 25
;;                          (group 1.0))
;;                (vertical 1.0
;;                          (summary 0.25 point)
;;                          (article 1.0)))))
;; (gnus-add-configuration
;;  '(summary
;;    (horizontal 1.0
;;                (vertical 25
;;                          (group 1.0))
;;                (vertical 1.0
;;                          (summary 1.0 point)))))

;; Two panes layout
;; (gnus-add-configuration
;;  '(article (vertical 1.0 (summary .35 point) (article 1.0))))


;; =====
;;  ERC
;; =====
(load "~/.emacs.d/.erc-auth") ;;(setq irc-freenode-nick-passwd "somepassword")


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
;; Exited emacs by accident too many times!!!
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x C-c C-x") 'save-buffers-kill-terminal)

;; Macbook keybord layout
(setq ns-function-modifier 'control)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; GUI
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Modeline information
(column-number-mode t)
;; (display-time-mode t)
;; (display-battery-mode t)

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
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Prevent backups from littering the file system
(setq backup-directory-alist '(("." . "~/emacsbackups")))

;; Parentheses
;;(electric-pair-mode t)
(show-paren-mode t)

;; Tabs handling
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4) ;; Defaults to 8

;; Scrolling
(setq scroll-conservatively 10)
(setq scroll-margin 3) ;;7

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

;; TODO(kleesc): Move this elsewhere
;; Javascript-mode tab length
(setq js-indent-level 2)

;; Mouse avoidance
;; (mouse-avoidance-mode 'banish)

;; Browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome")

;; Emacs server
(server-start)

;; exec-path and PATH
(add-to-list 'exec-path "~/bin/")
(setenv "PATH" (concat "/home/kenny/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/home/kenny/virtualenv/main/bin/pyls:" (getenv "PATH")))

;; ANSI Color in terminals
(ansi-color-for-comint-mode-on)

;; Advanced commands
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Font size (in 24, font is unusually bigger...)
(set-face-attribute 'default nil :height 105) ;; In 1/10 pt: 100*0.10 = 10pt


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
(load-theme 'molokai t)

;; Gimmicky Nyan cat (not practical on smaller screens)
;; (case window-system
;;   ((x w32) (nyan-mode)))

;; Yasnippet
;; (require 'yasnippet)
;; (yas-global-mode t)
;; (add-hook 'term-mode-hook
;;           (lambda() ;; Yas interferes with tab completion in ansi-term.
;;             (setq yas-dont-activate t)))
;;(yas/initialize)
;;(yas/load-directory "~/.emacs.d/packages/yasnippet-master/snippets")
;;(setq yas/prompt-functions '(yas/dropdown-prompt)) ;; Uses dropdown-list.el instead of OS window

;; Auto-complete
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; TODO(kleesc): Remove
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
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
;(flycheck-tip-use-timer 'verbose)

;; ;; Magit
;; (require 'magit)

;; Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; js2-mode 
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js-mode-hook 'js2-minor-mode) ;; Linting
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

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
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

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

;; HTML & CSS hex color
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; CC Mode
;; gdb
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
); gdb-show-main t)

;; Default global options
(setq c-default-style "linux")
(defvaralias 'c-basic-offset 'tab-width)

(add-hook 'java-mode-hook
	  (lambda ()
	    (c-set-style "java")))

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq c-basic-offset 8)))

;; Kernel files hook
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))

;; Go mode
(require 'go-mode)
(require 'go-guru)
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setenv "GOPATH" "/home/kenny/workspace/go")
(add-to-list 'exec-path "/home/kenny/workspace/go/bin")

;; go get -u github.com/nsf/gocode
(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (company-mode)
                          (set (make-local-variable 'company-backends) '(company-go))))
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))

;; go get golang.org/x/tools/cmd/...
;; go get github.com/rogpeppe/godef
;; go get golang.org/x/tools/cmd/goimports
;; go get golang.org/x/tools/cmd/guru
(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
; Go oracle (aka guru)
  (go-guru-hl-identifier-mode)
  ; (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Python
;; (setq python-shell-virtualenv-path "")
(add-hook 'python-mode-hook 'anaconda-mode)

;; Haskell mode
(require 'haskell-mode)

;; Yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Bbdb
;; Contact autocompletion with Bbdb
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-message)
;;(setq bbdb-update-records-p 'create)
;;(setq bbdb-ignore-message-alist '((("To" "CC") . "email@home")))
(setq bbdb-use-pop-up nil)
(setq bbdb-mua-pop-up nil)
;; size of the bbdb popup
(setq bbdb-pop-up-window-size 0.15)
(setq bbdb-mua-pop-up-window-size 0.15)
;; What do we do when invoking bbdb interactively
(setq bbdb-mua-update-interactive-p '(query . create))
;; Make sure we look at every address in a message and not only the
;; first one
(setq bbdb-message-all-addresses t)
;; use ; on a message to invoke bbdb interactively
(add-hook
 'gnus-summary-mode-hook
 (lambda ()
   (define-key gnus-summary-mode-map (kbd ";") 'bbdb-mua-edit-field)))

;; sr-speedbar
(setq sr-speedbar-skip-other-window-p t)
(setq sr-speedbar-right-side nil)
;; sr-speedbar-max-width
(global-set-key (kbd "<f5>") 'sr-speedbar-toggle)


;; ----------
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

;; Where the symlinks and database to system headers are located.
;; Doesn't seem to work at the moment (probably a helm-gtags issue).
;; Symlink required directories into project root (bit of a hack but works)
(setenv "GTAGSLIBPATH" "/home/kenny/.gtags/")

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

(define-key helm-gtags-mode-map (kbd "C-c g a") 
  'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; ggtags 
;; currently using helm-gtags...Because navigating results open new buffers
;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

;; (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;; (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;; (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;; (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;; (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;; (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
;; (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;;; init.el ends here
