;;; +system.el -*- lexical-binding: t; -*-

;; A collection of system utilty functionality


;;
;; Network manager
(use-package! enwc
  :config
  ;; TODO: Add custom keymap
  (setq enwc-default-backend 'nm)
  (defun nc/enwc ()
    "A custom enwc setup function that is compatible with popup rules."
    (interactive)
    (enwc-setup)
    (enwc-setup-buffer t)
    (enwc-scan t)
    (pop-to-buffer "*ENWC*"))
  (set-popup-rule! "*ENWC*" :size 0.4 :side 'bottom :select t :autosave t))


;;
;; Torrents
(use-package! transmission
  :config
  (setq  transmission-refresh-modes '(transmission-mode
                                      transmission-files-mode
                                      transmission-info-mode
                                      transmission-peers-mode))
  (set-popup-rule! "*transmission*" :size 0.4 :side 'bottom :select t :autosave t))


;;
;; Process manager
(use-package! proced
  :config
  (set-popup-rule! "*Proced*" :size 0.4 :side 'bottom :select t :autosave t))


;;
;; Disk usage
(use-package! disk-usage
  :config
  (map!
   :map disk-usage-mode-map
   :desc "Reset cache" :nmi "r" #'disk-usage-reset-cache
   :desc "Dired here" :nmi "D" (λ! () (dired default-directory))))

;;
;; Pulse audio control
(use-package! pulseaudio-control
  ;; TODO: Implement transient to control the separate functions?
  )
