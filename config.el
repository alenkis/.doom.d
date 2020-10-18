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
(add-hook 'json-mode-hook 'prettier-js-mode)

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

(use-package! flycheck-clj-kondo
  :after clojurescript-mode)

;; ;; First install the package:
;; (use-package flycheck-clj-kondo
;;   :ensure t)

;; ;; then install the checker as soon as `clojure-mode' is loaded
;; (use-package clojure-mode
;;   :ensure t
;;   :config
;;   (require 'flycheck-clj-kondo))
