(require 'ht)

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


;; various functions used for requests
(defun mx/well/get-azure-jwt (env)
  "Load the JWT token deposited via aws-login $env"
  (interactive "Menv: ")
  (let (
        (file-env
         (pcase env
           ("localhost" "development")
           (otherwise env))
         ))
    (f-read-text (s-lex-format "/Users/michael.xavier/${file-env}.jwt"))
    )
  )

(defun mx/well/get-arbiter-api (env)
  "Compute the base URL to use based on the environment"
  (interactive "Menv: ")
  (let ((production-boa "https://nlb.arbiter.us-east-2.boa.ps.pro.aws.wellit.io")
        )
    (pcase env
      ("production-boa" production-boa)
      ("production-boa-admin" production-boa)
      ("development-platform-services" "https://nlb.arbiter.us-east-2.ps.dev.aws.wellit.io")
      ("development-platform-services-sit" "https://nlb.arbiter.us-east-2.sit.ps.dev.aws.wellit.io")
      ("production-sams" "https://nlb.arbiter.us-east-2.sam.ps.pro.aws.wellit.io")
      ("localhost" "http://localhost:8000")
      (otherwise (s-lex-format "https://arbiter.${env}.well.co"))
      )
    )
  )

(defun mx/well/parse-cognito-token-file (env)
  "Parse the cognito token file for the given env"
  (interactive "Menv: ")
  (let* (
         (file-env
          (pcase env
            ("localhost" "development")
            (otherwise env)))
         (raw-file (f-read-text (s-lex-format "/Users/michael.xavier/${file-env}_cognito.json")))
         )
    (json-parse-string raw-file)
    )
  )

(defun mx/well/get-cognito-authorization (env)
  "Get the .authenticationResult.idToken value for the Authorization header"
  (interactive "Menv: ")
  (ht-get* (mx/well/parse-cognito-token-file env) "IdToken")
  )

(defun mx/well/get-cognito-access-token (env)
  "Get the .authenticationResult.accessToken value for the AccessToken header"
  (interactive "Menv: ")
  (ht-get* (mx/well/parse-cognito-token-file env) "AccessToken")
  )

;; https://emacs.stackexchange.com/a/51734
;; this ob evaluates the block as ifself, so it can be used as input
;; for another block

(defun org-babel-execute:passthrough (body params)
  body)

;; json output is json
(defalias 'org-babel-execute:json 'org-babel-execute:passthrough)

;; Enable ob-jq from jq-mode
(org-babel-do-load-languages 'org-babel-load-languages
                             '((jq . t)
                               (passthrough . t)))

;; Disable org element cache because it seems to have frequent issues.
(setq org-element-use-cache nil)
