;;;  -*- lexical-binding: t; -*-

;; lsp
;; lsp-ui
(with-eval-after-load 'lsp-mode
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-header nil
        lsp-file-watch-threshold 150000)
  (add-to-list 'lsp-file-watch-ignored "build")

  ;; remote
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "~/.pyenv/shims/pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote)))





;;
;; Formatting
(add-to-list '+format-on-save-enabled-modes 'python-mode t)
(use-package! py-autopep8
 :config
 ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
 (setq py-autopep8-options '("--max-line-length=79")))
