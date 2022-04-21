(setq org-directory "~/Dropbox/org/")

(defun org-archive-done-tasks ()
  "Archive done tasks in the current org mode file"
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

; hook into archiving items in org mode and save org
; buffers. otherwise, archive files don't automatically get saved and
; are easy to forget about
(advice-add 'org-archive-subtree-default :after #'org-save-all-org-buffers)

; Allow refile to work at deeper levels than default, otherwise copied from doom emacs' modules/lang/org/config.el
(setq org-refile-targets
        '((nil :maxlevel . 10)
          (org-agenda-files :maxlevel . 3))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

;; Set more generous timeouts in verb
(setq verb-babel-timeout 60.0)
;; but warn after 10
(setq verb-show-timeout-warning 10.0)
