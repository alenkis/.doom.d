;;; settings/setup-rust.el -*- lexical-binding: t; -*-

(use-package rustic
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-flycheck-setup-mode-line-p nil)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-linked-projects ["/Users/akis/projects/purescript/feedmanager/wasm/crate/Cargo.toml"])
  (setq lsp-rust-analyzer-inlay-hints-mode t)
  ;; :custom
  ;; (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :config
  (setq rustic-format-on-save t)

  (defun my-rustic-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'rustic-mode-hook #'my-rustic-mode-hook))
