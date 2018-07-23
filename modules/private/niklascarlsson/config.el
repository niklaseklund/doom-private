;; Mac specific setup
(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  )


;; General setup
(setq user-full-name    "Niklas Carlsson"
      user-mail-address "carlsson.niklas@gmail.com"

      +doom-modeline-buffer-file-name-style 'relative-from-project
      show-trailing-whitespace t
      ;; mu4e
      mu4e-maildir (expand-file-name "~/.mail")
      ;; Don't ask when killing emacs
      confirm-kill-emacs nil
      )
;; tabs
;; Python try to guess tab-width but it assumes wrong width sometimes, turn it
;; off to make it more consistent. I always want 4 spaces for my tabs.
(setq python-indent-guess-indent-offset nil)
;; maximize first frame
(set-frame-parameter nil 'fullscreen 'maximized)
;; remove trailing whitespaces (globaly)
(add-hook 'before-save-hook #'delete-trailing-whitespace)


;; Unimpaired functions
(defun evil-unimpaired/insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))
(defun evil-unimpaired/insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))


;; TRAMP
;; make tramp assume ssh to avoid typing it when connecting to remote host
(setq tramp-default-method "ssh")


;; Flyspell
;; this should be set to nil for performance
;; https://www.emacswiki.org/emacs/FlySpell
(setq flyspell-issue-message-flag nil)

;; Org-mode
;; customize org-settings
(after! org
  (setq outline-blank-line nil)
  (setq org-cycle-separator-lines 2)
  (setq org-log-done 'time))
;; Turn of highlight line in org-mode
(add-hook 'org-mode-hook (lambda ()
                           (hl-line-mode -1)))
;; turn of flycheck-mode
(add-hook 'org-mode-hook (lambda ()
                           (flycheck-mode -1)))
;; automatically redisplay images generated by babel
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
;; place latex-captions below figures and tables
(setq org-latex-caption-above nil)


;; Org-Noter
(def-package! org-noter
  :config
  (map!
   (:leader
     (:prefix "n"
       :desc "Org-noter-insert" :n "i" #'org-noter-insert-note))))
;; Setup
(setq org-noter-always-create-frame nil
      org-noter-auto-save-last-location t)


;; Magit
;; automatic spellchecking in commit messages
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)


;; Multi-Term
;; solve missing variables in terminal
(when IS-MAC
  (setenv "LC_CTYPE" "UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LANG" "en_US.UTF-8")
  )


;; LSP-Mode
(def-package! lsp-mode
  :commands (lsp-mode))


;; LSP-Company
(def-package! company-lsp
  :after lsp-mode)
(set-company-backend! '(c-mode c++-mode) '(company-lsp company-files company-yasnippet))
(after! lsp-mode
  (setq company-lsp-enable-snippet t)
  (setq company-lsp-cache-candidates nil)
  (setq company-lsp-async t))


;; LSP-Flycheck
(require 'lsp-ui-flycheck)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))
(add-hook 'c-mode-common-hook 'flycheck-mode) ;; Turn on flycheck for C++ buffers


;; GDB
;; Open debugging window style
(setq gdb-many-windows t)


;; Fill column indication
;; turn it off by default
(remove-hook! (text-mode prog-mode conf-mode) #'turn-on-fci-mode)

;; eshell
;; add fish-like autocompletion
(def-package! esh-autosuggest)
(add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
;; utilize completion from fish
(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))
;; aliases
(after! eshell
  (set-eshell-alias!
   "ff"  "counsel-projectile-find-file"
   "fd"  "counsel-projectile-find-dir"
   "l"   "ls -l"
   "la"  "ls -la"
   "d"   "dired $1"
   "gl"  "(call-interactively 'magit-log-current)"
   "gs"  "magit-status"
   "gc"  "magit-commit"
   "rg"  "rg --color=always $*"))


;; Dired
;; Make it possible to move files between two open Direds easily
(setq dired-dwim-target t)


;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; app/email
(after! mu4e
  ;; (setq mu4e-get-mail-command "~/mu/sync-mail")
  ;;       mu4e-update-interval 300) ;; update every 5 minutes
  (setq mu4e-bookmarks
        `(("maildir:/gmail.com/Inbox" "Inbox" ?i)
          ("maildir:/gmail.com/Drafts" "Drafts" ?d)
          ("flag:unread" "Unread messages" ?u)
          ("flag:flagged" "Starred messages" ?s)
          ("date:today..now" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)))

  (setq smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (set-email-account! "gmail.com"
                      '((mu4e-sent-folder       . "/gmail.com/Sent Mail")
                        (mu4e-drafts-folder     . "/gmail.com/Drafts")
                        (mu4e-trash-folder      . "/gmail.com/Trash")
                        (smtpmail-smtp-user     . "carlsson.niklas")
                        (user-mail-address      . "carlsson.niklas@gmail.com")
                        (mu4e-compose-signature . "---\nNiklas"))))


;; ccls
(def-package! ccls
  :commands (lsp-ccls-enable)
  :init
  :config
  (setq ccls-executable (expand-file-name "~/src/opensource/ccls/Release/ccls")
        ccls-cache-dir (concat doom-cache-dir ".ccls_cached_index")
        ccls-sem-highlight-method 'font-lock)
  (setq ccls-extra-args '("--log-file=/tmp/cc.log"))
  (setq ccls-extra-init-params
        '(:completion (:detailedLabel t) :xref (:container t)
                      :diagnostics (:frequencyMs 5000)))
  (set-company-backend! '(c-mode c++-mode) '(company-lsp))
  )
;; run ccls by default in C++ files
(defun ccls//enable ()
  (condition-case nil
      (lsp-ccls-enable)
    (user-error nil)))
  (use-package ccls
    :commands lsp-ccls-enable
    :init (add-hook 'c-mode-common-hook #'ccls//enable))


;; ;; Cquery
;; (def-package! cquery
;;   :hook ((c-mode c++-mode objc-mode) . +setup-cquery)
;;   :init
;;   (setq cquery-extra-init-params '(:index (:comments 2)
;;                                           :cacheFormat "msgpack"
;;                                           :completion (:detailedLabel t))
;;         cquery-sem-highlight-method 'overlay) ;; set to 'font-lock if highlighting slowly
;;   :config
;;   (setq cquery-executable "/home/niklascarlsson/src/opensource/cquery/build/release/bin/cquery"
;;         cquery-cache-dir (concat doom-cache-dir ".cquery_cached_index"))
;;   (setq cquery-extra-args '("--log-file=/tmp/cquery.log"))
;;   (defun +setup-cquery ()
;;     (setq-local company-transformers nil)
;;     (setq-local company-lsp-cache-candidates nil)
;;     (condition-case nil
;;         (lsp-cquery-enable)
;;       (user-error nil))))


;; ;; ccls new
;; (def-package! ccls
;;               :commands (lsp-ccls-enable)
;;               :init (add-hook! (c-mode c++-mode objc-mode) #'ccls//enable)
;;               :config
;;               (setq ccls-executable "/home/niklascarlsson/src/opensource/ccls/Release/ccls"
;;                     ccls-cache-dir (concat doom-cache-dir ".ccls_cached_index")
;;                     ccls-sem-highlight-method 'font-lock)
;;               ;; (ccls-use-default-rainbow-sem-highlight)
;;               (setq ccls-extra-args '("--log-file=/tmp/cc.log"))
;;               (setq ccls-extra-init-params
;;                     '(:clang (:extraArgs ("-D__cpp_deduction_guides=0" "-Wno-macro-redefined"))
;;                              :completion (:detailedLabel t)
;;                              :diagnostics (:frequencyMs 5000)
;;                              :index (:initialReparseForDependency :json-false)))
;;               (set-company-backend! '(c-mode c++-mode) '(company-lsp))
;;               )
;; ;; run ccls by default in C++ files
;; (defun ccls//enable ()
;;   (require 'ccls)
;;   (condition-case nil
;;       (lsp-ccls-enable)
;;     (user-error nil)))
;;   (use-package ccls
;;     :commands lsp-ccls-enable
;;     :init (add-hook 'c-mode-common-hook #'ccls//enable))
