;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;;;

;;; All buffers
(require 'undo-tree)
(global-undo-tree-mode)


;;; Environment
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; Fonts
(setq doom-font (font-spec :family "Menlo" :size 18 :weight 'normal))

(setq projectile-project-paths
      (let ((base-dir "~/projects/")
            (list-of-projects '("clojure"
                                "javascript"
                                "typescript"
                                "libraries"
                                "misc"
                                "react"
                                "rust"
                                "vue")))
        (mapcar (lambda (project) (concat base-dir project)) list-of-projects)))

(setq
 projectile-project-search-path projectile-project-paths)

;;; Org
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

;;; Dired
(map! :after dired
      :map dired-mode-map
      :localleader
      (:desc "Naigate to parent directory" :n "u" #'dired-up-directory
       :desc "Create an empty file" :n "f" #'dired-create-empty-file))


;;; JS/TS
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

(map! :leader
      :desc "toggle centered cursor" :n "t c" (λ! () (interactive) (centered-cursor-mode 'toggle)))

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

(defun setup-typescript-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (setq-default evil-surround-pairs-alist
  ;;     (push '(?t . evil-surround-ts-type) evil-surround-pairs-alist))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (prettier-js-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-typescript-mode)
(use-package! flycheck-clj-kondo
  :after clojurescript-mode)

(defun evil-surround-ts-type ()
  (let ((tname (evil-surround-read-from-minibuffer "" "")))
    (cons (format "%s<" (or tname ""))
          ">")))


;;; Clojure
(defun setup-clojure
    ()
  (interactive)
  (progn
    (add-hook 'before-save-hook 'cider-format-buffer t t)
    (setq cider-inspector-fill-frame t)))

(add-hook 'clojure-mode-hook 'setup-clojure)
(add-hook 'clojurescript-mode 'setup-clojure)

;; Similar to C-x C-e, but sends to REBL
(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

(map! :after cider
      :map clojure-mode-map
      :localleader
      (:desc "eval" :prefix "e"
       :desc "send last sexp to REBL" :n "E" #'rebl-eval-last-sexp
       :desc "defn at point to REBL" :n "P" #'rebl-eval-defun-at-point))

;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(map!
 :map clojure-mode-map
 :localleader
 (:prefix ("f" . "paredit")
  :desc "slurp" :n "s" #'paredit-forward-slurp-sexp
  :desc "barf" :n "b" #'paredit-forward-barf-sexp
  :desc "forward" :n "<right>" #'paredit-forward
  :desc "backward" :n "<left>" #'paredit-backward
  :desc "up" :n "<up>" #'paredit-forward-up
  :desc "down" :n "<down>" #'paredit-backward-down
  :desc "forward to matching sexp" :n "l" #'forward-sexp
  :desc "backward to matching sexp" :n "h" #'backward-sexp
  ;; nested prefix
  ;; (:prefix ("f" . "forward")
  ;;  :desc "slurp" :n "s" #'paredit-forward-slurp-sexp)
  ))

;;; Rust
(use-package rustic
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-flycheck-setup-mode-line-p nil)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-inlay-hints-mode t)
  :config
  (setq rustic-format-on-save t)

  (defun my-rustic-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-capf company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'rustic-mode-hook #'my-rustic-mode-hook))

;;; Haskell
(map!
 :map haskell-mode-map
 :localleader
 (:prefix ("r" . "repl")
  :desc "open" :n "o" #'+haskell/open-repl
  :desc "load" :n "l" #'haskell-process-load-file)
 (:prefix ("f" . "format")
  :desc "format buffer" :n "b" #'haskell-mode-stylish-buffer))

(setq haskell-stylish-on-save t)

;;; Elm

(defun setup-elm-mode ()
  (interactive)
  (elm-format-on-save-mode +1))

(add-hook 'elm-mode-hook #'setup-elm-mode)

;;; Purescript
(add-hook 'purescript-mode-hook 'inferior-psci-mode)
(require 'repl-toggle)
(require 'psci)
(add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci))

(defun purescript-format-on-save ()
  (interactive)
  (let ((file (buffer-file-name)))
    (shell-command-to-string (concat "purty --write " file))
    (revert-buffer :ignore-auto :noconfirm)))


(defun setup-purs-format ()
  (when (eq major-mode 'purescript-mode)
    (purescript-format-on-save)))

(add-hook 'after-save-hook 'setup-purs-format)

(map!
 :map purescript-mode-map
 :localleader
 (:prefix ("f" . "format")
  :desc "format buffer" :n "b" #'purescript-format-on-save))

;;; OCaml
(add-hook 'tuareg-mode-hook
          (lambda () (add-hook 'before-save-hook 'ocamlformat-before-save)))

(setq utop-command "opam config exec -- dune utop . -- -emacs")

(map!
 :map tuareg-mode-map
 :localleader
 (:prefix ("e" . "eval")
  :desc "buffer" :n "b" #'tuareg-eval-buffer
  :desc "phrase" :n "p" #'tuareg-eval-phrase)
 (:prefix ("i" . "insert")
  :desc "let" :n "l" #'tuareg-insert-let-form
  :desc "match" :n "m" #'tuareg-insert-let-form))


;;; DocView
(setq doc-view-resolution 300)

;;; ReScript
(customize-set-variable
 'lsp-rescript-server-command
 '("node" "/Users/akis/.vscode/extensions/chenglou92.rescript-vscode-1.2.1/server/out/server.js" "--stdio"))

(with-eval-after-load 'rescript-mode
  ;; Tell `lsp-mode` about the `rescript-vscode` LSP server
  (require 'lsp-rescript)
  ;; Enable `lsp-mode` in rescript-mode buffers
  (add-hook 'rescript-mode-hook 'lsp-deferred)
  ;; Enable display of type information in rescript-mode buffers
  (require 'lsp-ui)
  (add-hook 'rescript-mode-hook 'lsp-ui-doc-mode)

  (custom-set-variables
   '(lsp-rescript-prompt-for-build nil)
   '(lsp-ui-doc-position 'at-point)
   '(lsp-ui-doc-show-with-cursor t)
   '(lsp-ui-doc-show-with-mouse t)
   )


  ;; Terrible hack to disable annoying LSP warning
  (let ((lsp-rescript-handlers (lsp--client-notification-handlers (ht-get lsp-clients 'rescript-vscode))))
    (puthash "rescript/compilationFinished" #'ignore lsp-rescript-handlers))


  ;; disable automatic closing of single quotes
  (sp-local-pair '(rescript-mode) "'" "'" :actions nil)
  )
