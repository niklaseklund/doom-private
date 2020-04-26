;;; +system.el -*- lexical-binding: t; -*-

;;
;; Directory editor
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
;; Package manager
(use-package! guix
  :defer t
  :config
  (set-popup-rule! "\\*Guix Packages*" :ignore t)
  (set-popup-rule! "\\*Guix Package Info*" :side 'bottom :size 0.8 :vslot 10)
  (set-popup-rule! "\\*Guix REPL\\*" :side 'bottom :size 0.3 :vslot 5))


;;
;; Version control
(add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)
(add-hook 'ediff-prepare-buffer-hook #'outline-show-all)
;; add submodules to magit-status
(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil))


;;
;; Disk usage
(use-package! disk-usage
  :defer t)


;;
;; Bluetooth
(use-package! bluetooth
  :defer t
  :config
  (advice-add 'bluetooth-list-devices :before '+bluetooth-pop-to-buffer-a)
  (set-popup-rule! "*Bluetooth*" :size 0.3 :side 'bottom :select t :autosave t)
  (map!
   :map bluetooth-mode-map
   :desc "Connect" :n "c" #'bluetooth-connect
   :desc "Disconnect" :n "x" #'bluetooth-disconnect))


;;
;; Transmission
(use-package! transmission
  :defer t
  :config
  (setq transmission-refresh-modes '(transmission-mode
                                      transmission-files-mode
                                      transmission-info-mode
                                      transmission-peers-mode))
  (set-popup-rule! "*transmission*" :size 0.3 :side 'bottom :select t :autosave t)
  (with-eval-after-load 'transmission
    (advice-add 'transmission :before '+transmission-start-daemon-a)
    (setq transmission-refresh-modes '(transmission-mode
                                       transmission-files-mode
                                       transmission-info-mode
                                       transmission-peers-mode)
          transmission-refresh-interval 1)))


;;; Video
(use-package! ivy-youtube
  :defer t
  :config
  (setq ivy-youtube-key (+pass-get-secret "web/youtube/api-key")
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"
        ivy-youtube-play-at (executable-find "vlc")
        ivy-youtube-history-file (concat doom-local-dir "ivy-youtube-history")))


;;
;; Conflicts
(use-package! emacs-conflict
  :after dired)


;;
;; Calendar
;; Show week numbers in calendar
(use-package! calendar
  :config
  (setq calendar-week-start-day 1
        calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-function-name-face)))


;;
;; Password manager
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
  ;; Keybindings
  (map!
   :map pass-mode-map

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
;; Notifications
(use-package! alert
  :config
  (setq alert-default-style 'libnotify)
  (add-hook 'compilation-start-hook '+compile-start-time-h)
  (add-hook 'compilation-finish-functions '+compile-notify-finish-h))
