;;;  -*- lexical-binding: t; -*-

;; lsp
;; lsp-ui
(with-eval-after-load 'lsp-mode
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-header nil))
;; company
;; (with-eval-after-load 'company-lsp
  ;; (setq company-lsp-enable-snippet t))
;; lsp-flycheck
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable t)))
  (add-hook 'c-mode-common-hook 'flycheck-mode)) ;; Turn on flycheck for C++ buffers
;; lsp-formatting
(add-hook 'before-save-hook (lambda ()  (when (eq major-mode 'python-mode) (lsp-format-buffer))))
