;;;  -*- lexical-binding: t; -*-


;;
;; UI
(setq user-full-name    "Niklas Carlsson"
      user-mail-address "carlsson.niklas@gmail.com")

;; looks
(setq doom-modeline-height 40
      +doom-modeline-buffer-file-name-style 'relative-from-project
      doom-theme 'doom-dracula
      +workspaces-switch-project-function #'ignore)

;; prettify modes
(setq +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))
(set-frame-parameter nil 'fullscreen 'maximized)

;; host os configuration
(load! "+functions")
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


;;
;; Mail
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp")


;;
;; Screencast
(def-package! gif-screencast
  :defer t
  :config
  (with-eval-after-load 'gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f12>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f11>") 'gif-screencast-stop))
)

;; make window larger
(setq command-log-mode-window-size 60)


;;
;; Writing
(def-package! writeroom-mode
  :after org
  :init
  (setq writeroom-width 100)
  (add-hook 'writeroom-mode-hook #'my/writeroom)
  :config
  ;; create keybinding for toggling zen-writing
  (defun my/writeroom ()
    (interactive)
    (if writeroom-mode
        ;; enter
        (progn (git-gutter-mode -1)
               (visual-line-mode))
      ;; exit
      (progn (git-gutter-mode)
             (visual-line-mode -1))))
  (map! :localleader
        :map org-mode-map
        :desc "Toggle zen writing" :n "z" #'writeroom-mode))


;;
;; Remote editing
(with-eval-after-load 'tramp-sh
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


;; Let the scratch buffer have elisp major mode by default
;; if set to t it has the same mode as previous buffer
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

;;
;; Projects
(setq projectile-enable-caching nil
      projectile-project-search-path '("~/src/" "~/opensource"))


;;
;; Windows (not the operating system)
(after! org
  (set-popup-rule! "^\\*Org Agenda.*\\*$" :size 0.5 :side 'right :select t :ttl nil :autosave t)
  (set-popup-rule! "^CAPTURE.*\\.org$" :size 0.4 :side 'bottom :select t :autosave t))


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
              (start-process "switch-to-normal" nil "xkb-switch" "-s" normal-mode-keyboard-layout))))



;;
;; Dired/Ranger
(after! ranger
  ;; Make the result of find-name-dired not open in a popup window but rather
  ;; reuse the current one
  (set-popup-rule! "^\\*Find\\*" :ignore t)
  ;; Run ranger-refresh directly after find-name-dired has executed. For some
  ;; reason it doesn't play entirely well with ranger so to avoid having to
  ;; manually refresh I advice the function.
  (defun my/find-name-dired (orig-fun &rest args)
    (apply orig-fun args)
    (ranger-refresh))
  (advice-add 'find-name-dired :around #'my/find-name-dired)
  ;; Make it easier to move between windows
  (map!
   (:map ranger-mode-map
     "C-h" #'evil-window-left
     "C-l" #'evil-window-right
     "C-k" #'evil-window-up
     "C-j" #'evil-window-down)))


;;
;; Flycheck
(add-hook 'text-mode-hook (lambda ()
                            (flycheck-mode -1)))
(add-hook 'org-mode-hook (lambda ()
                           (flycheck-mode -1)))


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
(after! cmake
  (set-docsets! 'cmake-mode "CMake"))
;; add archwiki to online providers
(add-to-list '+lookup-provider-url-alist '("ArchWiki" . "https://wiki.archlinux.org/index.php?search=%s"))


;;
;; Mode file association
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-hook 'octave-mode-hook (lambda ()
                            (flycheck-mode -1)))
(add-to-list 'auto-mode-alist '("\\.MD\\'" . markdown-mode))


;;
;; Load other config files
(load! "+agenda")
(load! "+bindings")
(load! "+brain")
(load! "+debug")
(load! "+eshell")
(load! "+lsp")
(load! "+org")

;;   :config
