;;;  -*- lexical-binding: t; -*-

;; TODO divide into different sections
;; 1) user settings
;; 2) emacs core settings
;; 3) doom looks settings
;; 4) doom core settings
;; 5) doom package settings
;; 6) custom package settings
;; 7) load my own "module" files

;; Load functions
(load! "+functions")

;;
;; User
(setq user-full-name    "Niklas Carlsson"
      user-mail-address "carlsson.niklas@gmail.com")

;;
;; Looks
(setq doom-modeline-height 40
      +doom-modeline-buffer-file-name-style 'relative-from-project
      doom-theme 'doom-dracula
      show-trailing-whitespace nil)

;;
;; General
(setq confirm-kill-emacs nil
      projectile-project-search-path '("~/src/" "~/opensource")
      +workspaces-switch-project-function #'ignore
      python-indent-guess-indent-offset nil)
(set-frame-parameter nil 'fullscreen 'maximized)

;;
;; Host configuration
(when (my/os-match "ARCH")
  (if (my/multi-screen-setup-p)
      (setq doom-font (font-spec :family "Roboto Mono" :size 16)
          doom-big-font (font-spec :family "Roboto Mono" :size 22)
          doom-variable-pitch-font (font-spec :family "Iosevka Term" :size 14))
    (setq doom-font (font-spec :family "Roboto Mono" :size 18)
            doom-big-font (font-spec :family "Roboto Mono" :size 36)
            doom-variable-pitch-font (font-spec :family "Iosevka Term" :size 18)))
  (font-put doom-font :weight 'semi-light)
  (setq x-super-keysym 'meta
        x-alt-keysym 'alt))
(when (my/os-match "Ubuntu")
    (setq doom-font (font-spec :family "Roboto Mono" :size 14)))
(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
               mac-option-modifier 'alt
               mac-command-modifier 'meta)

;;
;; Calendar
;; Show week numbers in calendar
  (setq calendar-week-start-day 1
        calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-function-name-face))


;;
;; Mail config
(setq send-mail-function 'sendmail-send-it
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp")


;;
;; Writeroom
(add-hook 'writeroom-mode-hook (lambda ()
                                 (progn
                                   (visual-line-mode 1)
                                   (hl-line-mode -1))))

;;
;; TRAMP
;; make tramp assume ssh to avoid typing it when connecting to remote host
(setq tramp-default-method "ssh")
;; solution for getting around a server with warning message about a not fully
;; functional terminal. This is due to the fact that tramp is set to "dumb"
(with-eval-after-load 'tramp-sh
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

(with-eval-after-load 'tramp
;; Docker-Tramp
(require 'docker-tramp))

;; Ediff
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)


;; Command-log-mode
(setq command-log-mode-window-size 60)


;; Flyspell
;; this should be set to nil for performance
;; https://www.emacswiki.org/emacs/FlySpell
(setq flyspell-issue-message-flag nil)
;; aspell is the successor to ispell so let's use
(setq ispell-program-name "aspell")

;; Flycheck
;; disable using hooks
(add-hook 'text-mode-hook (lambda ()
                            (flycheck-mode -1)))
(add-hook 'org-mode-hook (lambda ()
                           (flycheck-mode -1)))


;; Elisp
(setq +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))

;; Let the scratch buffer have elisp major mode by default
;; if set to t it has the same mode as previous buffer
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

;; Screencast
(def-package! gif-screencast
  :defer t
  :config
  (with-eval-after-load 'gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f12>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f11>") 'gif-screencast-stop))
)


;; Projectile
(setq projectile-enable-caching nil)


;; Pop-rule
(after! org
  (set-popup-rule! "^\\*Org Agenda.*\\*$" :size 0.5 :side 'right :vslot 1  :select t :quit t   :ttl nil :modeline nil :autosave t)
  (set-popup-rule! "^CAPTURE.*\\.org$"    :size 0.4 :side 'bottom          :select t                                  :autosave t))


;; Magit
;; automatic spellchecking in commit messages
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
;; mitigate terminal is dumb
(setenv "EDITOR" "emacsclient")
;; submodules
(with-eval-after-load 'magit
(magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil))


;; Automatically switch back to English in normal mode
;; Set default normal/insert-mode language to English
(let* ((normal-mode-keyboard-layout "us")
       (insert-mode-keyboard-layout normal-mode-keyboard-layout))
  ;; Add entry hook
  (add-hook 'evil-insert-state-entry-hook
            ;; switch language when entering insert mode to insert mode layout
            (lambda () (quiet! (shell-command (concat "xkb-switch -s " insert-mode-keyboard-layout)))))
  ;; Add exit hook
  (add-hook 'evil-insert-state-exit-hook
            ;; save current insert mode layout and reset layouot to english
            (lambda () (setq insert-mode-keyboard-layout (shell-command-to-string "xkb-switch -p"))
              (quiet! (shell-command (concat "xkb-switch -s " normal-mode-keyboard-layout))))))


;;
;; eshell
;; fish-like auto-suggestions
(def-package! esh-autosuggest
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
  (when (and (executable-find "fish")
             (require 'fish-completion nil t))
    (global-fish-completion-mode)))
;; aliases
(after! eshell
  (set-eshell-alias!
   "ff"  "+ivy/projectile-find-file"
   "fd"  "counsel-projectile-find-dir"
   "/p" "+ivy/project-search"
   "/d" "+ivy/project-search-from-cwd"
   "d"   "deer $1"
   "l"   "ls -l"
   "la"  "ls -la"
   "gl"  "(call-interactively 'magit-log-current)"
   "gs"  "magit-status"
   "gc"  "magit-commit"
   "gbD" "my/git-branch-delete-regexp $1"
   "gbS" "my/git-branch-match $1"
   "rg"  "rg --color=always $*"
   "bat" "my/eshell-bat $1")
  (setenv "PAGER" "cat"))


;; ;; deer/ranger
;; (after! dired
;;   (setq ranger-show-hidden t))


;; Snipe
(evil-snipe-override-mode 1)


;; Avy
;; Make avy operate on all visable windows
(setq avy-all-windows t)


;; Autoformat in C++ files using clang-format
;; (add-hook 'c++-mode-hook #'+format|enable-on-save)
(add-hook 'before-save-hook (lambda ()  (when (eq major-mode 'python-mode) (lsp-format-buffer))))


;; Matlab files (use octave mode)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-hook 'octave-mode-hook (lambda ()
                            (flycheck-mode -1)))

;; Markdown
(add-to-list 'auto-mode-alist '("\\.MD\\'" . markdown-mode))

;;
;; Load other config files
(load! "+bindings")
(load! "+org")
(load! "+lsp")
(load! "+debug")

;;   :config
