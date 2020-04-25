;;; +system.el -*- lexical-binding: t; -*-

;; A collection of system utilty functionality


;;
;; Disk usage
(use-package! disk-usage
  :config
  (map!
   :map disk-usage-mode-map
   :desc "Reset cache" :nmi "r" #'disk-usage-reset-cache
   :desc "Dired here" :nmi "D" (λ! () (dired default-directory))))


;;
;; Bluetooth
(use-package! bluetooth
  :config

  ;; Ensure bluetooth buffer is delegated to the popup system.
  (defadvice! +popup--bluetooth-pop-to-buffer ()
    "Use `pop-to-buffer' instead of `switch-to-buffer' to open buffer.'"
    :before #'bluetooth-list-devices
    (pop-to-buffer "*Bluetooth*"))

  ;; Configure popup rule
  (set-popup-rule! "*Bluetooth*" :size 0.3 :side 'bottom :select t :autosave t)

  ;; Keybindings
  (map!
   :map bluetooth-mode-map
   :desc "Connect" :n "c" #'bluetooth-connect
   :desc "Disconnect" :n "x" #'bluetooth-disconnect
   :desc "Information" :n "RET" #'bluetooth-show-device-info
   :desc "Remove" :n "X" #'bluetooth-remove-device
   (:prefix ("t" . "Toggle")
     :desc "Power" :n  "p" #'bluetooth-toggle-power
     :desc "Pairable" :n "P" #'bluetooth-toggle-pairable
     :desc "Discoverable" :n "d" #'bluetooth-toggle-discoverable
     :desc "Blocked" :n "b" #'bluetooth-toggle-blocked
     :desc "Trusted" :n "t" #'bluetooth-toggle-trusted)
   (:prefix ("d" . "Discover")
     :desc "Begin" :n "b" #'bluetooth-start-discovery
     :desc "Stop" :n "s" #'bluetooth-stop-discovery)))

(use-package! guix
  :config
  (set-popup-rule! "\\*Guix Packages*" :ignore t)
  (set-popup-rule! "\\*Guix Package Info*" :side 'bottom :size 0.8 :vslot 10)
  (set-popup-rule! "\\*Guix REPL\\*" :side 'bottom :size 0.3 :vslot 5))

;;
;; Conflicts
(use-package! emacs-conflict
  :after dired)

;;
;; Notifications
(use-package! alert
  :config
  (setq alert-default-style 'libnotify)

  (defun nc/compile-start-time (_process)
    "Record the start time of the compilation."
    (setq-local compile-start-time (time-to-seconds)))
  (add-hook 'compilation-start-hook 'nc/compile-start-time)

  (defun nc/compile-notify-finish (_buffer string)
    "Conditionally show a notification when a compilation finishes.
Always notify about compilations that are failing. Notify about successful ones
if they have been running for enough time."
    (let* ((duration-threshold 10)
           (compilation-end-time (time-to-seconds))
           (compile-duration (float-time (time-subtract compilation-end-time compile-start-time))))
      (if (string-match "^finished" string)
          (when (> compile-duration duration-threshold)
            (alert "Compilation finished OK!" :title "Compilation Successful" :severity 'moderate :category 'compile :id 'compile-ok))
        (alert "Compilation Failed" :title "Compilation Failed" :severity 'high :category 'compile :id 'compile-fail))))
  (add-hook 'compilation-finish-functions 'nc/compile-notify-finish))
