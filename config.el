;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


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

(setq-default evil-surround-pairs-alist
  (push '(?t . evil-surround-ts-type) evil-surround-pairs-alist))

;;; Clojure
(defun add-clj-format-before-save 
    ()
    (interactive)
    (add-hook 'before-save-hook 'cider-format-buffer t t))

(add-hook 'clojure-mode-hook 'add-clj-format-before-save)
(add-hook 'clojurescript-mode 'add-clj-format-before-save)

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
       :desc "defn at point to REBL" :n "P" #'rebl-eval-defun-at-point
       ))

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
  (setq racer-rust-src-path
        (concat
         (string-trim
          (shell-command-to-string "rustc --print sysroot"))
         "/lib/rustlib/src/rust/src"))
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-flycheck-setup-mode-line-p nil)
  :config
  (setq rustic-format-on-save t))

;;; Haskell
(map!
 :map haskell-mode-map
 :localleader
 (:prefix ("r" . "repl")
  :desc "open" :n "o" #'+haskell/open-repl
  :desc "load" :n "l" #'haskell-process-load-file))

;;; Elm

(defun setup-elm-mode ()
  (interactive)
  (elm-format-on-save-mode +1))

(add-hook 'elm-mode-hook #'setup-elm-mode)
