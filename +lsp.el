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

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'before-save-hook #'lsp-format-buffer))

;; ;; specific language server configurations
;; ;; ;; lsp-python configuration
;; (with-eval-after-load 'lsp-clients
;;   (setq lsp-clients-python-settings
;;         ;; https://github.com/tomv564/LSP/issues/239
;;         (plist-put lsp-clients-python-settings
;;                    :configurationSources ["pycodestyle"])
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pycodestyle.enabled t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pycodestyle.maxLineLength 80)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.yapf.enabled nil)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.jedi_definition.follow_imports t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.jedi_definition.follow_builtin_imports t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.jedi_signature_help.enabled t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.jedi_symbols.enabled t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.matchDir "[^\\.].*")
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pyls_mypy.live_mode nil)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pydocstyle.enabled t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.ignore ["D209"])
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.ignore ["D209"])
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.convention "pep257")
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.select ["D209"])
;;         ))
;; ;; https://github.com/Seeker1911/dotfiles/blob/b28aa7c10c205cb1d3ad94d27787118e0ec68d3b/coc-settings.json

;; https://github.com/nasyxx/emacs.d/blob/f91655b31610cdcff4d896dacea6482862ba8d13/README.org
;; (with-eval-after-load 'lsp-clients
;;   (setq lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :configurationSources ["pydocstyle" "pycodestyle" "pyflakes" "pylint"])
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pydocstyle.enabled t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.yapf.enabled nil)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.jedi_definition.follow_imports t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.jedi_definition.follow_builtin_imports t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.jedi_signature_help.enabled t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.jedi_symbols.enabled t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pydocstyle.matchDir "[^\\.].*")
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pyls_mypy.live_mode nil)))
