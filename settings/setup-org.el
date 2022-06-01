;;; settings/setup-org.el -*- lexical-binding: t; -*-

(after! org
  (add-to-list 'org-capture-templates
             '("w" "Work-related task" entry
               (file "~/org/work.org")
               "* TODO %?" :empty-lines 1))
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

(setq jiralib-url "https://bonsai-hq.atlassian.net")
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
       :desc "browse-issue" :n "b" #'org-jira-browse-issue
       :desc "progress-issue" :n "p" #'org-jira-progress-issue
       :desc "progress-issue-next" :n "n" #'org-jira-progress-issue-next
       :desc "assign-issue" :n "a" #'org-jira-assign-issue
       :desc "refresh-issue" :n "r" #'org-jira-refresh-issue
       :desc "refresh-issues-in-buffer" :n "R" #'org-jira-refresh-issues-in-buffer
       :desc "copy-current-issue-key" :n "k" #'org-jira-copy-current-issue-key
       :desc "create-issue" :n "c" #'org-jira-create-issue
       :desc "create-subtask" :n "s" #'org-jira-create-subtask
       :desc "add-comment" :n "m" #'org-jira-add-comment
       :desc "todo-to-jira" :n "t" #'org-jira-todo-to-jira))
