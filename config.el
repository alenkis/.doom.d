;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq
 projectile-project-search-path '("~/projects"))

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

(setq display-line-numbers-type 'relative)
(setq-default typescript-indent-level 2)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'json-mode-hook 'prettier-js-mode)

(setq emmet-expand-jsx-className? t)
(add-hook 'js2-mode-hook 'emmet-mode)
(map! :leader
      :desc "Expand emmet line" "e e" #'emmet-expand-line) ;

(add-hook 'fundamental-mode 'centered-cursor-mode)
