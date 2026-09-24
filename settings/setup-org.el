;;; settings/setup-org.el -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/org/"
        org-agenda-files '("~/org/work.org"
                           "~/org/notes.org"
                           "~/org/career.org"))

  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-redeadline 'time
        org-log-reschedule 'time)

  (setq org-tag-alist
        '(("shipped" . ?s)
          ("design" . ?d)
          ("incident" . ?i)
          ("crossteam" . ?x)
          ("mentoring" . ?m)
          ("promo" . ?p)
          ("claude" . ?c)))

  (setq org-agenda-custom-commands
        '(("b" . "Brag doc")
          ("bq" "Closed — last 90 days"
           agenda ""
           ((org-agenda-span 90)
            (org-agenda-start-day "-90d")
            (org-agenda-start-with-log-mode '(closed))
            (org-agenda-show-log 'only)
            (org-agenda-archives-mode t)))
          ("bh" "Closed — last 180 days"
           agenda ""
           ((org-agenda-span 180)
            (org-agenda-start-day "-180d")
            (org-agenda-start-with-log-mode '(closed))
            (org-agenda-show-log 'only)
            (org-agenda-archives-mode t)))
          ("bs" "Tagged accomplishments"
           tags "shipped|crossteam|mentoring|incident|design"
           ((org-agenda-archives-mode t)))
          ("bc" "Career file — promo, feedback, growth"
           tags "+LEVEL<=2"
           ((org-agenda-files '("~/org/career.org"))
            (org-agenda-overriding-header "Career")))))

  (setq org-capture-templates
        '(("w" "Work TODO" entry
           (file "~/org/work.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :empty-lines 1)

          ("t" "General note / TODO" entry
           (file "~/org/notes.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :empty-lines 1)

          ("c" "Career")
          ("cc" "Checkin entry" entry
           (file+headline "~/org/career.org" "Checkins")
           "** %^{Cycle|2026 Mid-Year|2026 End-Year}\n:PROPERTIES:\n:CREATED: %U\n:END:\n*** %^{Section|Accomplishments|Growth Areas|Goals|Manager Feedback}\n%?"
           :empty-lines 1)
          ("c1" "1:1 note" entry
           (file+headline "~/org/career.org" "1:1 Notes")
           "** %<[%Y-%m-%d %a]> %^{With} 1:1\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
           :empty-lines 1)
          ("cf" "Feedback item" entry
           (file+headline "~/org/career.org" "Feedback Log")
           "** %<[%Y-%m-%d %a]> %^{From} — %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :empty-lines 1)
          ("cg" "Growth goal" entry
           (file+headline "~/org/career.org" "Growth Goals")
           "** TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :empty-lines 1)
          ("cp" "Promo prep note" entry
           (file+headline "~/org/career.org" "Promo Prep")
           "** %<[%Y-%m-%d %a]> %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :empty-lines 1)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))

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

(advice-add 'org-agenda-goto :after
            (lambda (&rest _args)
              (org-narrow-to-subtree)))

(setq org-modern-fold-stars
      '(("▶" . "▼") ("▷" . "▽") ("▸" . "▾") ("▹" . "▿") ("►" . "▼")))

(add-hook 'org-mode-hook #'org-modern-mode)

;; Org JIRA
(setq jiralib-url "https://wealthsimple.atlassian.net")
(map!
 :after org
 :map org-mode-map
 :localleader
 (:prefix ("j" . "jira")
          (:prefix ("g" . "get")
           :desc "get-projects" :n "p" #'org-jira-get-projects
           :desc "get-boards" :n "b" #'org-jira-get-boards
           :desc "get-issues-by-board" "r" #'org-jira-get-issues-by-board
           :desc "get-issues" "i" #'org-jira-get-issues
           :desc "get-issues-from-custom-jql" "j" #'org-jira-get-issues-from-custom-jql
           :desc "get-issues-headonly" "h" #'org-jira-get-issues-headonly
           :desc "get-issues-by-fixversion" :n "f" #'org-jira-get-issues-by-fixversion
           :desc "get-subtasks" :n "s" #'org-jira-get-subtasks)
          (:prefix ("u" . "update")
           :desc "update-issue" :n "i" #'org-jira-update-issue
           :desc "update-comment" :n "c" #'org-jira-update-comment
           :desc "update-worklogs-from-org-clocks" :n "w" #'org-jira-update-worklogs-from-org-clocks)
          (:prefix ("c" . "create")
           :desc "create-issue" :n "i" #'org-jira-create-issue
           :desc "create-subtask" :n "s" #'org-jira-create-subtask)
          (:prefix ("r" . "refresh")
           :desc "refresh-issue" :n "i" #'org-jira-refresh-issue
           :desc "refresh-issues-in-buffer" :n "b" #'org-jira-refresh-issues-in-buffer)

          :desc "browse-issue" :n "b" #'org-jira-browse-issue
          :desc "progress-issue" :n "p" #'org-jira-progress-issue
          :desc "progress-issue-next" :n "n" #'org-jira-progress-issue-next
          :desc "assign-issue" :n "a" #'org-jira-assign-issue
          :desc "copy-current-issue-key" :n "k" #'org-jira-copy-current-issue-key
          :desc "add-comment" :n "m" #'org-jira-add-comment
          :desc "todo-to-jira" :n "t" #'org-jira-todo-to-jira))
