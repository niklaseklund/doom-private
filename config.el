;;;  -*- lexical-binding: t; -*-


;;
;; UI
(setq user-full-name    "Niklas Carlsson"
      user-mail-address "niklas.carlsson@posteo.net")

;; looks
(setq doom-modeline-height 40
      +doom-modeline-buffer-file-name-style 'relative-from-project
      doom-theme 'doom-nord
      +workspaces-switch-project-function #'ignore)

;; prettify modes
(setq +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))

;; host os configuration
(when (string= (system-name) "archbook")
  (setq doom-font (font-spec :family "Fira Code" :size 16)
        doom-big-font-increment 8
        ;; doom-variable-pitch-font (font-spec :family "EtBembo" :size 22)
        )
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

;;
;; Elisp
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

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
        :desc "Quit" :m "q" #'pass-quit))


;;
;; Version control
(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)
(with-eval-after-load 'magit
  ;; add submodules to magit-status
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil))


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
       :desc "Show conflicts" :n "s" #'+emacs-conflict/show-conflicts-dired-at-point
       :desc "Resolve marked files or at point" :n "r" #'emacs-conflict-resolve-conflict-dired))
   ;; Define or redefine dired bindings
   (:map dired-mode-map
     :desc "Ediff files" :n "=" #'+dired/ediff-files
     :desc "Up" :n "h" #'dired-up-directory
     :desc "Down" :n "l" #'dired-find-file
     :desc "Find" :n "f" #'find-file
     :desc "Recent files" :n "zz" #'dired-recent-open
     :desc "Home" :n "gh" (λ! () (find-file "~")))))

;;
;; Documentation
;; right docsets for major-modes
(after! python
  (set-docsets! 'python-mode "Pandas"))

;; add archwiki to online providers
(add-to-list '+lookup-provider-url-alist '("ArchWiki" "https://wiki.archlinux.org/index.php?search=%s"))
;; remove providers that I don't use
(setq +lookup-provider-url-alist (assoc-delete-all "Google images" +lookup-provider-url-alist))
(setq +lookup-provider-url-alist (assoc-delete-all "Google maps" +lookup-provider-url-alist))


;;
;; Mode file association
(add-to-list 'auto-mode-alist '("\\.MD\\'" . markdown-mode))

;;
;; Load other config files
(load! "+app")
(load! "+bindings")
(load! "+brain")
(load! "+code")
(load! "+eshell")
(load! "+org")
(load! "+mail")
(load! "+system")
