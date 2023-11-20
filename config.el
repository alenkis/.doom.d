;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;; All buffers
(require 'undo-tree)
(global-undo-tree-mode)

(setq treemacs-position 'right)

;; Enable folding
(setq lsp-enable-folding t)

(setq lsp-ui-doc t)

(use-package centered-cursor-mode
  :init
  (global-centered-cursor-mode +1))

;;; environment
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
                                "haskell"
                                "misc"
                                "react"
                                "rust"
                                "vue")))
        (mapcar (lambda (project) (concat base-dir project)) list-of-projects)))

(setq
 projectile-project-search-path projectile-project-paths)

(load! "settings/setup-org")
(load! "settings/setup-flycheck")
(load! "settings/setup-elm")
(load! "settings/setup-clojure")
(load! "settings/setup-haskell")
(load! "settings/setup-rust")
(load! "settings/setup-purescript")

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

(defun my/use-bonsai-tsserver ()
  "Sets tsserver executable to Bonsai specific one"
  (let (path)
    (setq path "/Users/akis/projects/bonsai/.yarn/sdks/typescript/lib/tsserver")
    (setq-default lsp-clients-typescript-server-args `("--stdio" "--tsserver-path" ,path))))


(setq-default lsp-clients-typescript-server-args `("--stdio" "--tsserver-path", "/Users/akis/projects/bonsai/.yarn/sdks/typescript/lib/tsserver"))

(defun setup-typescript-mode ()
  (interactive)
  (tide-mode -1)
  (lsp)
  (my/use-bonsai-tsserver)
  (flycheck-mode +1)
  (setq lsp-typescript-tsdk "/Users/akis/projects/bonsai/.yarn/sdks/typescript/lib/tsserver")
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (setq-default evil-surround-pairs-alist
  ;;     (push '(?t . evil-surround-ts-type) evil-surround-pairs-alist))
  ;; (eldoc-mode +1)
  ;; (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (prettier-js-mode +1)

  (require 'lsp-ui)

  (require 'eldoc)
  (global-eldoc-mode -1)

  (custom-set-variables
   '(lsp-ui-doc-position 'at-point)
   '(lsp-ui-doc-include-signature t)
   '(lsp-ui-doc-show-with-cursor t)
   '(lsp-ui-doc-show-with-mouse t)
   '(lsp-ui-doc-header nil)
   '(lsp-eldoc-enable-hover nil)
   '(lsp-eldoc-render-all nil)
   )
  )

(defun setup-mhtml-mode ()
  (interactive)
  (format-all-mode nil))

(add-hook 'mhtml-mode-hook #'setup-mhtml-mode)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-typescript-mode)
(defun evil-surround-ts-type ()
  (let ((tname (evil-surround-read-from-minibuffer "" "")))
    (cons (format "%s<" (or tname ""))
          ">")))

;;; Copilot
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

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


;;; Go
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook
          (lambda ()
            (setq-local +lookup-references-functions '(lsp-find-references))))
;;; DocView
(setq doc-view-resolution 300)

(custom-set-variables
 '(lsp-rescript-prompt-for-build nil)
 '(lsp-ui-doc-position 'bottom)
 '(lsp-ui-doc-show-with-cursor t)
 '(lsp-ui-doc-show-with-mouse t)
 )

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



  ;; Terrible hack to disable annoying LSP warning
  (let ((lsp-rescript-handlers (lsp--client-notification-handlers (ht-get lsp-clients 'rescript-vscode))))
    (puthash "rescript/compilationFinished" #'ignore lsp-rescript-handlers))


  ;; disable automatic closing of single quotes
  (sp-local-pair '(rescript-mode) "'" "'" :actions nil)
  )

