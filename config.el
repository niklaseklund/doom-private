;;;  -*- lexical-binding: t; -*-


;;
;; UI
(setq user-full-name    "Niklas Carlsson"
      user-mail-address "niklas.carlsson@posteo.net")

;; looks
(setq doom-modeline-height 40
      +doom-modeline-buffer-file-name-style 'relative-from-project
      doom-theme 'doom-dracula
      +workspaces-switch-project-function #'ignore)

;; prettify modes
(setq +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))
;; (set-frame-parameter nil 'fullscreen 'maximized)

;; host os configuration
(load! "+functions")
(when (string= (system-name) "archbook")
  (if (nc/multi-screen-setup-p)
      (setq doom-font (font-spec :family "Fira Code" :size 14)
            doom-big-font-increment 8
            doom-variable-pitch-font (font-spec :family "EtBembo" :size 20))
    (setq doom-font (font-spec :family "Fira Code" :size 16)
          doom-big-font-increment 8
          doom-variable-pitch-font (font-spec :family "EtBembo" :size 22)))
  (font-put doom-font :weight 'semi-light)
  (setq x-super-keysym 'meta
        x-alt-keysym 'alt))
(when (string= (system-name) "u445bfa80-2ca8")
  (setq doom-font (font-spec :family "Iosevka Term SS04" :size 16)
        doom-big-font-increment 8
        doom-variable-pitch-font (font-spec :family "EtBembo" :size 18))
  (font-put doom-font :weight 'semi-light))


;; Define meta and super keys
(setq x-super-keysym 'super
        x-meta-keysym  'meta)

;;
;; Core Emacs

;; Show week numbers in calendar
(setq calendar-week-start-day 1
      calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-function-name-face))

;; Don't bother
(setq confirm-kill-emacs nil)
;; Save customizations elsewhere
(setq custom-file (expand-file-name "custom.el" doom-etc-dir))
(load custom-file)


;;
;; Screencast
(use-package! gif-screencast
  :defer t
  :config
  (with-eval-after-load 'gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f12>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f11>") 'gif-screencast-stop)))

;; make window larger
(setq command-log-mode-window-size 60)


;;
;; Writing
;; (use-package! writeroom-mode
;;   :init
;;   (setq writeroom-width 100)
;;   :config
;;   (map! :localleader
;;         :map org-mode-map
;;         :desc "Toggle zen writing" :n "z" #'writeroom-mode))


;;
;; Remote editing
(with-eval-after-load 'tramp-sh
  ;; Create persistent connections
  (customize-set-variable
   'tramp-ssh-controlmaster-options
   (concat
    "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
    "-o ControlMaster=auto -o ControlPersist=yes"))
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil)
  ;; Add the remote host path
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; solution for getting around a server with warning message about a not fully
  ;; functional terminal. This is due to the fact that tramp is set to "dumb"
  ;; Found a similar problem to mine here:
  ;; http://emacs.1067599.n8.nabble.com/problem-getting-files-under-SunOS-from-cygwin-td277890.html
  ;; there is also an example there about an interactive user input version if I ever need that :)
  (defconst my-tramp-press-return-prompt-regexp
    "\\(-  (press RETURN)\\)\\s-*"
    "Regular expression matching my login prompt request.")

  (defun my-tramp-press-return-action (proc vec)
    "Enter \"?\^M\" to send a carriage return."
    (save-window-excursion
      (message "%s" vec)
      (with-current-buffer (tramp-get-connection-buffer vec)
        (tramp-message vec 6 "\n%s" (buffer-string))
        ;; The control character for Enter is ^M
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Ctl_002dChar-Syntax.html#Ctl_002dChar-Syntax
        (tramp-send-string vec "?\^M"))))

  (add-to-list 'tramp-actions-before-shell
               '(my-tramp-press-return-prompt-regexp my-tramp-press-return-action)))


;;
;; Elisp

;; Let the scratch buffer have elisp major mode by default
;; if set to t it has the same mode as previous buffer
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)
;; Enable lispy in elisp repl
(add-hook 'ielm-mode-hook 'lispy-mode)
;; I want the default lispy-bindings
;; (push 'lispy +evil-collection-disabled-list)

;;
;; Projects
(after! projectile
  (setq projectile-enable-caching t
        projectile-project-search-path '("~/src/" "~/opensource")
        ;; Follow suggestion to reorder root functions to find the .projectile file
        ;; https://old.reddit.com/r/emacs/comments/920psp/projectile_ignoring_projectile_files/
        projectile-project-root-files-functions #'(projectile-root-top-down
                                                   projectile-root-top-down-recurring
                                                   projectile-root-bottom-up
                                                   projectile-root-local))
  (add-to-list 'projectile-project-root-files ".projectile"))


;;
;; Windows (not the operating system)
(after! org
  (set-popup-rule! "^CAPTURE.*\\.org$" :size 0.4 :side 'bottom :select t :autosave t))


;;
;; Pass
(after! pass
  ;; enter evil mode
  (add-to-list 'evil-motion-state-modes 'pass-mode)
  ;; disable snipe to have access to those keys
  (push 'pass-mode evil-snipe-disabled-modes)
  ;; don't show the bindings
  (setq pass-show-keybindings nil
        password-store-password-length 20)
  ;; define the looks
  (set-popup-rule! "*Password-Store*" :size 0.3 :side 'left :select t :autosave t)
  ;; Let's make the password shown directly
  (add-hook 'pass-view-mode-hook 'pass-view-toggle-password)

  ;;
  ;; Keybindings
  ;; Pass-mode
  (map!
   :map pass-mode-map
   :m "C-k" #'evil-window-up
   :m "C-j" #'evil-window-down
   :m "C-h" #'evil-window-left
   :m "C-l" #'evil-window-right
   (:desc "Copy" :prefix "y"
     :desc "Copy password" :m "y" #'pass-copy
     :desc "Copy field" :m "f" #'pass-copy-field
     :desc "Copy username" :m "u" #'pass-copy-username
     :desc "Copy username" :m "U" #'pass-copy-url)
   :desc "Insert" :m "i" #'pass-insert
   :desc "Insert generated" :m "I" #'pass-insert-generated
   :desc "Rename" :m "r" #'pass-rename
   :desc "Next entry" :m "j" #'pass-next-entry
   :desc "Previous entry" :m "k" #'pass-prev-entry
   (:desc "Extra keys" :prefix "g"
     :desc "Next directory" :m "j" #'pass-next-directory
     :desc "Previous directory" :m "k" #'pass-prev-directory
     :desc "Refresh" :m "r" #'pass-update-buffer)
   :desc "Open entry" :m "o" #'pass-view
   :desc "OTP options" :m "Options" #'pass-otp-options
   :desc "Delete entry" :m "d" #'pass-kill
   :desc "Go to entry" :m "f" #'pass-goto-entry)
  ;; Pass-view-mode
  (map! :localleader
        :map pass-view-mode-map
        :desc "Toggle password" :m "t" #'pass-view-toggle-password
        :desc "View qr-code" :m "Q" #'pass-view-qrcode
        :desc "Copy password" :m "y" #'pass-view-copy-password
        :desc "Quit" :m "q" #'pass-quit)
  ;; TODO: improve the pass-quit function to properly clean-up the window layout
  )


;;
;; Version control
;; spell check commit messages
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
;; mitigate dumb terminals
(setenv "EDITOR" "emacsclient")
;; add submodules to magit-status
(with-eval-after-load 'magit
(magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil))
;; improve diff of org-mode files
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)


;;
;; Multi-language
;; automatic switch-back to English layout in normal mode
(let* ((normal-mode-keyboard-layout "us")
       (insert-mode-keyboard-layout normal-mode-keyboard-layout))
  ;; Will work on remote, but requires xkb-switch to be installed locally
  (when (executable-find "xkb\-switch")
    ;; Add entry hook
    (add-hook 'evil-insert-state-entry-hook
              ;; switch language when entering insert mode to insert mode layout
              (lambda () (start-process "switch-to-previous-language" nil "xkb-switch" "-s" insert-mode-keyboard-layout)))
    ;; Add exit hook
    (add-hook 'evil-insert-state-exit-hook
              ;; save current insert mode layout and reset layout to English
              (lambda () (setq insert-mode-keyboard-layout (with-temp-buffer
                                                        (call-process "xkb-switch" nil t "-p")
                                                        (goto-char (point-min))
                                                        (string-trim-right (buffer-substring-no-properties (point) (line-end-position)))))
                (start-process "switch-to-normal" nil "xkb-switch" "-s" normal-mode-keyboard-layout)))))





;;
;; Media player
(use-package! emms
  :config
  (require 'emms-setup)
  (setq emms-stream-info-backend 'vlc))


;;
;; Dired
(use-package! dired-recent
  :after dired
  :config
  ;; Enable recent directories in dired
  (setq dired-recent-directories-file
        (expand-file-name "dired-history" doom-etc-dir))
  (dired-recent-mode 1))
(use-package! dired-subtree
  :after dired
  :config
  (map!
   (:map dired-mode-map
     :desc "Toggle subtree" :n [tab] #'dired-subtree-toggle
     :desc "Cycle subtree" :n "<backtab>" #'dired-subtree-cycle
     :desc "Close subtree" :n "<C-tab>" #'dired-subtree-remove)))
(use-package! dired-narrow
  :after dired
  :config
  (map!
   (:localleader
     :map dired-mode-map
     :desc "Narrow buffer fuzzy" :n "n" #'dired-narrow-fuzzy
     :desc "Narrow buffer fuzzy" :n "N" #'dired-narrow-regex)))
(after! dired
  ;; Define localleader bindings
  (map!
   (:localleader
     :map dired-mode-map
     :desc "Search recursively" :n "s" #'find-dired
     (:prefix-map ("g" . "Go to")
       :desc "Project root" :n "p" (λ! () (find-file (projectile-project-root))))
     ;; Bindings for conflicts
     (:prefix-map ("c" . "Conflicts")
       :desc "Show conflicts" :n "s" #'+emacs-conflict-show-conflicts-dired-at-point
       :desc "Resolve marked files or at point" :n "r" #'emacs-conflict-resolve-conflict-dired))
   ;; Define or redefine dired bindings
   (:map dired-mode-map
     :desc "Ediff files" :n "=" #'nc/ediff-files
     :desc "Up" :n "h" #'dired-up-directory
     :desc "Down" :n "l" #'dired-find-file
     :desc "Find" :n "f" #'find-file
     :desc "Recent files" :n "zz" #'dired-recent-open
     :desc "Home" :n "gh" (λ! () (find-file "~")))))


;;
;; Disable flycheck
(add-hook! 'text-mode-hook (lambda ()
                             (flycheck-mode -1)))

;;
;; Command Log
(use-package! command-log-mode
  :commands global-command-log-mode
  :config
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode nil
        command-log-mode-is-global t
        command-log-mode-window-size 50))


;;
;; Auto-formatting
;; (add-hook 'c++-mode-hook #'+format|enable-on-save)
(add-hook 'before-save-hook (lambda ()  (when (eq major-mode 'python-mode) (lsp-format-buffer))))
(setq show-trailing-whitespace nil)


;;
;; Documentation
;; right docsets for major-modes
(after! python
  (set-docsets! 'python-mode "Python 3" "NumPy" "SciPy" "Pandas"))
(after! dockerfile
  (set-docsets! 'dockerfile-mode "Docker"))
;; add archwiki to online providers
(add-to-list '+lookup-provider-url-alist '("ArchWiki" "https://wiki.archlinux.org/index.php?search=%s"))
;; remove providers that I don't use
(setq +lookup-provider-url-alist (assoc-delete-all "Google images" +lookup-provider-url-alist))
(setq +lookup-provider-url-alist (assoc-delete-all "Google maps" +lookup-provider-url-alist))


;;
;; Mode file association
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-hook 'octave-mode-hook (lambda ()
                            (flycheck-mode -1)))
(add-to-list 'auto-mode-alist '("\\.MD\\'" . markdown-mode))


;;
;; Pop-ups
(set-popup-rule! "^\\*Customize Group:*" :side 'right :size 0.4)


;;
;; Feeder
(use-package! elfeed
  :config
  (setq elfeed-db-directory "~/sync/elfeed/db")
  ;; Gotta make doom treat elfeed-search-buffer as real. Otherwise exiting a
  ;; show buffer doesn't bring me back to the feed, but instead the latest
  ;; "real" buffer.
  (defun +elfeed-buffer-p (buf)
    "Return non-nil if BUF is a `elfeed-mode' buffer."
    (with-current-buffer buf
      (or
       (derived-mode-p 'elfeed-search-mode))))
  (add-hook 'doom-real-buffer-functions #'+elfeed-buffer-p)
  (add-hook 'elfeed-search-update-hook #'hide-mode-line-mode)

  ;; Customize show entry
  (defun nc/elfeed-search-show-entry (orig-fn &rest args)
    (apply orig-fn args)
    (hide-mode-line-mode)
    (writeroom-mode 1)
    (visual-line-mode))
  (advice-add 'elfeed-search-show-entry :around #'nc/elfeed-search-show-entry))


;; Improve defining feeds
(use-package! elfeed-org
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/sync/elfeed/elfeed.org")))


;;
;; PDF
(after! pdf-tools
  ;; customize midnight-mode colors
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
  (map!
   :map pdf-view-mode-map
   :desc "pdf-occur" :nmi "C-s" #'pdf-occur))

;;
;; Common Lisp
(add-hook 'lisp-mode-hook #'lispy-mode)
(after! sly
  (add-to-list 'sly-contribs 'sly-retro nil #'eq))


;;
;; Help/Documentation
(defun cmake-doc ()
  (interactive)
  (setq-local dash-docs-docsets '("CMake")))
(add-hook 'cmake-mode-hook 'cmake-doc)

(defun docker-doc ()
  (interactive)
  (setq-local dash-docs-docsets '("Docker")))
(add-hook 'dockerfile-mode-hook 'docker-doc)


;;
;; Load other config files
(load! "+agenda")
(load! "+app")
(load! "+bindings")
(load! "+brain")
(load! "+chat")
(load! "+dap")
(load! "+eshell")
(load! "+lsp")
(load! "+org")
(load! "+mail")
(load! "+system")
(load! "+wip")
(load! "+daemon")
