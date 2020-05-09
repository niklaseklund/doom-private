;;; +code.el -*- lexical-binding: t; -*-

;;
;;; LSP
(with-eval-after-load 'lsp-mode
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-header nil
        lsp-file-watch-threshold 150000)
  (add-to-list 'lsp-file-watch-ignored "build"))

;;
;; DAP
(with-eval-after-load 'dap-mode
  (set-company-backend! 'dap-ui-repl-mode 'company-dap-ui-repl)
  (add-hook 'dap-ui-repl-mode-hook #'+dap/company-h)
  (add-hook 'dap-stopped-hook #'+dap/show-debug-windows-h)
  (add-hook 'dap-terminated-hook #'+dap/hide-debug-windows-h))


;;
;;; Python
(advice-add #'dap-python--pyenv-executable-find :around #'+dap/python-poetry-a)
(advice-add #'+python/open-repl :around #'+python/open-poetry-repl-a)
(after! python
  (set-docsets! 'python-mode "Pandas"))
(map! :map inferior-python-mode-map
      :desc "Search history"    "C-s" #'comint-history-isearch-backward-regexp)


;;
;; C++
(add-to-list '+format-on-save-enabled-modes 'c++-mode t)


;;
;;; Emacs Lisp
(add-hook 'ielm-mode-hook 'lispy-mode)
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)


;;
;; Matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(add-hook 'octave-mode-hook (lambda ()
                            (flycheck-mode -1)))
