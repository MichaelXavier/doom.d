(setq org-directory "~/Dropbox/org/")

;; take back C-c SPC
(map! :mode org-mode-map
      "C-c SPC" #'ace-jump-mode)

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

;; org markdown supports strikethrough with + characters
(sp-local-pair 'org-mode "+" "+")
