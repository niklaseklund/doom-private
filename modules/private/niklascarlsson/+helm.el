;;; private/niklascarlsson/+helm.el -*- lexical-binding: t; -*-
(defun +helm--set-prompt-display (pos)
  "TODO"
  (let (beg state region-active m)
    (with-selected-window (minibuffer-window)
      (setq beg (save-excursion (vertical-motion 0 (helm-window)) (point))
            state evil-state
            region-active (region-active-p)
            m (mark t)))
    (when region-active
      (setq m (- m beg))
      ;; Increment pos to handle the space before prompt (i.e `pref').
      (put-text-property (1+ (min m pos)) (+ 2 (max m pos))
                         'face
                         (list :background (face-background 'region))
                         header-line-format))
    (put-text-property
     ;; Increment pos to handle the space before prompt (i.e `pref').
     (+ 1 pos) (+ 2 pos)
     'face
     (if (eq state 'insert)
         'underline
       ;; Don't just use 'cursor, this can hide the current character.
       (list :inverse-video t
             :foreground (face-background 'cursor)
             :background (face-background 'default)))
     header-line-format)))


(def-package! helm-mode
  :defer t
  :after-call pre-command-hook
  :config
  (helm-mode +1))


(def-package! helm
  :after helm-mode
  :preface
  (setq helm-candidate-number-limit 50
        ;; Remove extraineous helm UI elements
        helm-display-header-line nil
        helm-mode-line-string nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Don't override evil-ex's completion
        helm-mode-handle-completion-in-region nil
        ;; Default helm window sizes
        helm-display-buffer-default-width nil
        helm-display-buffer-default-height 0.25
        ;; When calling `helm-semantic-or-imenu', don't immediately jump to
        ;; symbol at point
        helm-imenu-execute-action-at-once-if-one nil
        ;; disable special behavior for left/right, M-left/right keys.
        helm-ff-lynx-style-map nil)

  (setq helm-default-prompt-display-function #'+helm--set-prompt-display)

  :config
  (set-popup-rule! "^\\*helm" :vslot -100 :size 0.22 :ttl nil)

  ;; Hide the modeline
  (defun +helm|hide-mode-line (&rest _)
    (with-current-buffer (helm-buffer-get)
      (unless helm-mode-line-string
        (hide-mode-line-mode +1))))
  (add-hook 'helm-after-initialize-hook #'+helm|hide-mode-line)
  (advice-add #'helm-display-mode-line :override #'+helm|hide-mode-line)
  (advice-add #'helm-ag-show-status-default-mode-line :override #'ignore))

(def-package! helm-lsp
  :after lsp-mode)

;; Bindings
(map! 
 (:after helm
   (:map helm-map
     [left]     #'left-char
     [right]    #'right-char
     "C-S-n"    #'helm-next-source
     "C-S-p"    #'helm-previous-source
     "C-j"      #'helm-next-line
     "C-k"      #'helm-previous-line
     "C-S-j"    #'helm-next-source
     "C-S-k"    #'helm-previous-source
     "C-f"      #'helm-next-page
     "C-S-f"    #'helm-previous-page
     "C-u"      #'helm-delete-minibuffer-contents
     "C-w"      #'backward-kill-word
     "C-r"      #'evil-paste-from-register ; Evil registers in helm! Glorious!
     "C-s"      #'helm-minibuffer-history
     "C-b"      #'backward-word
     ;; Swap TAB and C-z
     [tab]      #'helm-execute-persistent-action
     "C-z"      #'helm-select-action)))
