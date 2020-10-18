;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq projectile-project-paths
      (let ((base-dir "~/projects/")
            (list-of-projects '("clojure"
                                "javascript"
                                "libraries"
                                "misc"
                                "react"
                                "rust"
                                "typescript"
                                "vue")))
        (mapcar (lambda (project) (concat base-dir project)) list-of-projects)))

(setq
 projectile-project-search-path projectile-project-paths)

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                         :time-grid t
                                         :scheduled today)
                                  (:name "Due today"
                                         :deadline today)
                                  (:name "Important"
                                         :priority "A")
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due soon"
                                         :deadline future)))
  :config
  (org-super-agenda-mode))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq display-line-numbers-type 'relative)
(setq-default typescript-indent-level 2)
(setq-default js2-basic-offset 2)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode 'prettier-js-mode)
(add-hook 'json-mode-hook 'prettier-js-mode)

(add-to-list 'auto-mode-alist '("\\.vue\\'" . rjsx-mode))

(setq emmet-expand-jsx-className? t)
(add-hook 'js2-mode-hook 'emmet-mode)
(map! :leader
      :desc "Expand emmet line" "e e" #'emmet-expand-line) ;

(add-hook 'fundamental-mode 'centered-cursor-mode)

(map! :leader
      :desc "Terminal (ansi)" "o t"
      '(lambda ()
         (interactive)
         (progn
           (let ((name "terminal"))
             (evil-window-vsplit)
             (if (get-buffer name)
                 (switch-to-buffer name)
               (progn
                 (ansi-term "/bin/zsh")
                 (rename-buffer "terminal")))))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(use-package! flycheck-clj-kondo
  :after clojurescript-mode)

(defun add-clj-format-before-save 
    ()
    (interactive)
    (add-hook 'before-save-hook 'cider-format-buffer t t))

(add-hook 'clojure-mode-hook 'add-clj-format-before-save)
(add-hook 'clojurescript-mode 'add-clj-format-before-save)

;; RUST
(use-package rustic
  :init
  (setq racer-rust-src-path
        (concat
         (string-trim
          (shell-command-to-string "rustc --print sysroot"))
         "/lib/rustlib/src/rust/src"))
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-flycheck-setup-mode-line-p nil)
  :config
  (setq rustic-format-on-save t))
