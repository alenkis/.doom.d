;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


;; Enable folding
(setq lsp-enable-folding t)

(setq lsp-ui-doc t)

(use-package centered-cursor-mode
  :demand t
  :config
  (global-centered-cursor-mode +1))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x))
          (exec-path-from-shell-initialize)))

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

;;; Mise
(use-package mise
  :config
  (add-hook 'after-init-hook #'global-mise-mode))

;;; Claude Code IDE
(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'eat
        claude-code-ide-enable-mcp-server t
        claude-code-ide-no-flicker t
        claude-code-ide-window-side 'right
        claude-code-ide-window-width 100)
  (claude-code-ide-emacs-tools-setup))

;;; eat (used by claude-code-ide only)
(after! eat
  (setq eat-term-name "xterm-256color"))

;;; eshell
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local display-line-numbers-type 'relative)
            (display-line-numbers-mode 1)))

(after! eshell
  (setq eshell-banner-message ""))

(defun my/eshell-short-branch ()
  (cl-destructuring-bind (status . output)
      (doom-call-process "git" "symbolic-ref" "-q" "--short" "HEAD")
    (if (equal status 0)
        (format " [%s]" (replace-regexp-in-string "^[^/]+/" "" output))
      "")))

(defun my/eshell-prompt-fn ()
  (concat (if (bobp) "" "\n")
          (propertize (file-name-nondirectory
                       (directory-file-name (eshell/pwd)))
                      'face '+eshell-prompt-pwd)
          (propertize (my/eshell-short-branch)
                      'face '+eshell-prompt-git-branch)
          (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
          " "))

(setq eshell-prompt-function #'my/eshell-prompt-fn
      eshell-prompt-regexp "^[^\n]* λ ")

(defcustom my/claude-worktree-root "~/code/worktrees"
  "Root directory under which per-project worktrees live."
  :type 'directory)

(defun my/claude--repo-name (root)
  "Canonical repository name for ROOT.
Resolves via the shared git dir so linked worktrees report the main
repository's name instead of their own branch-slug directory."
  (let* ((status 1)
         (common (with-output-to-string
                   (with-current-buffer standard-output
                     (setq status
                           (call-process "git" nil '(t nil) nil
                                         "-C" (expand-file-name root)
                                         "rev-parse" "--path-format=absolute"
                                         "--git-common-dir")))))
         (common (string-trim common)))
    (if (or (not (eq status 0)) (string-empty-p common))
        (file-name-nondirectory (directory-file-name root))
      (file-name-nondirectory
       (directory-file-name
        (if (string-suffix-p "/.git" common)
            (file-name-directory (directory-file-name common))
          ;; bare repo: /path/to/foo.git -> foo
          (string-remove-suffix ".git" common)))))))

(defun my/claude--branch-slug (branch)
  "Filesystem-safe form of BRANCH."
  (replace-regexp-in-string "/" "-" branch))

(defun my/claude--worktree-path (project branch)
  "Compute worktree path for PROJECT + BRANCH."
  (expand-file-name
   (concat project "/" (my/claude--branch-slug branch))
   my/claude-worktree-root))

(defun my/claude--workspace-name (project branch)
  "Workspace name for PROJECT + BRANCH.
Mirrors the <parent>/<project> form Doom uses when it uniquifies
workspace names, so worktree workspaces read consistently."
  (concat project "/" (my/claude--branch-slug branch)))

(defcustom my/claude-branch-prefix "alenkis/apf-"
  "Default prefix inserted into the branch prompt."
  :type 'string)

(defcustom my/claude-repos
  '("~/code/auth-service"
    "~/code/backend-library-monorepo"
    "~/code/front-end-monorepo"
    "~/code/llm-service"
    "~/code/mcplocker"
    "~/code/risk-service"
    "~/code/wealthsimple")
  "Repositories offered when creating a Claude worktree."
  :type '(repeat directory))

(defun my/claude--read-repo ()
  "Prompt for a repository from `my/claude-repos'.
Defaults to the current buffer's repository when it is a known one."
  (let* ((alist (mapcar (lambda (p)
                          (cons (file-name-nondirectory (directory-file-name p))
                                (expand-file-name p)))
                        my/claude-repos))
         (current (when-let* ((pr (project-current nil)))
                    (my/claude--repo-name (project-root pr))))
         (default (car (assoc current alist)))
         (name (completing-read
                (if default (format "Repo (default %s): " default) "Repo: ")
                alist nil t nil nil default)))
    (or (cdr (assoc name alist))
        (user-error "Unknown repository: %s" name))))

(defun my/claude-worktree-new (root branch)
  "Create worktree in ROOT for new BRANCH, copy env, launch Claude.
ROOT is chosen from `my/claude-repos'; the branch prompt is pre-filled
with `my/claude-branch-prefix'.
Path: <my/claude-worktree-root>/<repo>/<branch-slashes-as-dashes>."
  (interactive (list (my/claude--read-repo)
                     (read-string "New branch: " my/claude-branch-prefix)))
  (unless (and (file-directory-p root)
               (eq 0 (call-process "git" nil nil nil "-C" (expand-file-name root)
                                   "rev-parse" "--git-dir")))
    (user-error "Not a git repository: %s" root))
  (let* ((project (my/claude--repo-name root))
         (path (my/claude--worktree-path project branch))
         (buf (get-buffer-create "*worktree*")))
    (make-directory (file-name-directory path) t)
    (with-current-buffer buf (erase-buffer))
    (unless (zerop (call-process "git" nil buf t "-C" root
                                 "worktree" "add" "-b" branch path))
      (pop-to-buffer buf)
      (user-error "git worktree add failed"))
    (dolist (f '(".env" ".env.local" ".envrc" ".claude/settings.local.json"))
      (let ((s (expand-file-name f root))
            (d (expand-file-name f path)))
        (when (file-exists-p s)
          (make-directory (file-name-directory d) t)
          (copy-file s d t))))
    (when (fboundp 'projectile-add-known-project)
      (projectile-add-known-project path))
    ;; Switch first so the buffers opened below belong to the new workspace.
    (when (and (bound-and-true-p persp-mode) (fboundp '+workspace-switch))
      (+workspace-switch (my/claude--workspace-name project branch) t))
    (let ((default-directory (file-name-as-directory path)))
      (dired path)
      (claude-code-ide))))

(map! :leader
      (:prefix ("k" . "claude")
       :desc "Menu"          "k" #'claude-code-ide-menu
       :desc "New worktree"  "w" #'my/claude-worktree-new
       :desc "List sessions" "l" #'claude-code-ide-list-sessions
       ;; Project-scoped, unlike `claude-code-ide-toggle-recent', which falls
       ;; back to the globally last-accessed session in another workspace.
       :desc "Toggle"        "t" #'claude-code-ide-toggle
       :desc "Switch buffer" "b" #'claude-code-ide-switch-to-buffer
       :desc "Stop"          "q" #'claude-code-ide-stop))

;;; Terminal (eshell)
(defun my/eshell-here ()
  "Open eshell at buffer's directory. Fallback to ~ if none."
  (interactive)
  (let ((default-directory
         (or (when (buffer-file-name)
               (file-name-directory (buffer-file-name)))
             (and default-directory
                  (not (string-prefix-p "*" (buffer-name)))
                  default-directory)
             (expand-file-name "~/"))))
    (+eshell/here)))

(map! :leader
      :desc "Terminal (here)"    "o t" #'my/eshell-here
      :desc "Terminal (project)" "o T" #'+eshell/toggle)

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

(after! lsp-javascript
  (advice-add 'lsp-clients-typescript-server-path :override
              (lambda () nil)))

(defun setup-typescript-mode ()
  (interactive)
  ;; (tide-mode -1)
  (lsp)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (setq-default evil-surround-pairs-alist
  ;;     (push '(?t . evil-surround-ts-type) evil-surround-pairs-alist))
  ;; (eldoc-mode +1)
  ;; (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`

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

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-typescript-mode)
(defun evil-surround-ts-type ()
  (let ((tname (evil-surround-read-from-minibuffer "" "")))
    (cons (format "%s<" (or tname ""))
          ">")))

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


(let ((mise-shims (expand-file-name "~/.local/share/mise/shims")))
  (when (file-directory-p mise-shims)
    (add-to-list 'exec-path mise-shims)
    (setenv "PATH" (concat mise-shims path-separator (getenv "PATH")))))

(after! lsp-mode
  (dolist (client '(rubocop-ls ruby-ls sorbet-ls steep-ls ruby-syntax-tree-ls))
    (add-to-list 'lsp-disabled-clients client))
  (setq lsp-ruby-lsp-use-bundler t))
