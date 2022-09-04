;; custom keymapping
(map!
 "C-w" #'backward-kill-word
 "C-s" #'swiper
 "C-x C-m" #'execute-extended-command
 "C-x C-k" #'kill-region
 "C-S-p" #'edwina-select-previous-window
 "C-S-n" #'edwina-select-next-window
 "C-S-k" #'kill-whole-line
 "C-;" #'iedit-mode
 "C-/" #'undo-fu-only-undo
 "C-?" #'undo-fu-only-redo
 ;; unbind mail compose
 "C-x m" nil)

;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'bu 'browse-url)
(defalias 'ft 'find-tag)

;; smartparens bindings
;; https://github.com/Fuco1/smartparens/wiki/Example-configuration
(map! :map smartparens-mode-map
      "C-M-f" #'sp-forward-sexp
      "C-M-b" #'sp-backward-sexp
      "C-M-d" #'sp-down-sexp
      "C-M-a" #'sp-backward-down-sexp
      "C-S-a" #'sp-beginning-of-sexp
      "C-S-d" #'sp-end-of-sexp
      "C-M-e" #'sp-up-sexp
      "C-M-u" #'sp-backward-up-sexp
      "C-M-t" #'sp-transpose-sexp
      "C-M-n" #'sp-next-sexp
      "C-M-p" #'sp-previous-sexp
      "C-M-k" #'sp-kill-sexp
      "C-M-w" #'sp-copy-sexp
      "M-<delete>" #'sp-unwrap-sexp
      "M-<backspace>" #'sp-backward-unwrap-sexp
      "C-<right>" #'sp-forward-slurp-sexp
      "C-<left>" #'sp-forward-barf-sexp
      "C-M-<left>" #'sp-backward-slurp-sexp
      "C-M-<right>" #'sp-backward-barf-sexp
      "M-D" #'sp-splice-sexp
      "C-M-<delete>" #'sp-splice-sexp-killing-forward
      "C-M-<backspace>" #'sp-splice-sexp-killing-backward
      "C-S-<backspace>" #'sp-splice-sexp-killing-around
      "C-]" #'sp-select-next-thing-exchange
      "C-<left_bracket>" #'sp-select-previous-thing
      "C-M-]" #'sp-select-next-thing
      "M-F" #'sp-forward-symbol
      "M-B" #'sp-backward-symbol
      "H-t" #'sp-prefix-tag-object
      "H-p" #'sp-prefix-pair-object
      "H-s c" #'sp-convolute-sexp
      "H-s a" #'sp-absorb-sexp
      "H-s e" #'sp-emit-sexp
      "H-s p" #'sp-add-to-previous-sexp
      "H-s n" #'sp-add-to-next-sexp
      "H-s j" #'sp-join-sexp
      "H-s s" #'sp-split-sexp
      "C-c SPC" #'ace-jump-mode
      "C-+" #'default-text-scale-increase
      "C--" #'default-text-scale-decrease
      "C-0" #'default-text-scale-reset)

(map! :map emacs-lisp-mode-map
      ")" #'sp-up-sexp)

;; managing windows
(map! :map winner-mode-map
      "C-M-S-<left>" #'winner-undo
      "C-M-S-<right>" #'winner-redo)

;; Previously hitting escape 3 times focuses the current window and
;; closes the others. I almost always hit it by accident since my
;; escape and ctrl are the same key
(global-unset-key "\e\e\e")

;; Remove the whole line when you hit Ctrl-K instead of just clearing it
(setq kill-whole-line t)

;; assume all new files are modified so you can save them
(defun assume-new-is-modified ()
  (when (not (file-exists-p (buffer-file-name)))
    (set-buffer-modified-p t)))

                                        ; this breaks file-templates expansion
                                        ; (add-hook! 'find-file-hooks #'assume-new-is-modified)

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; Silly function that comes in handy once in a blue moon
(defun reverse-region (start end)
  "region to reverse"
  (interactive "r")
  (let ((reversed (concat (reverse (string-to-list (buffer-substring-no-properties start end))))))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert reversed) )) )

(defun save-buffer-always ()
  "Save the buffer even if it is not modified. Useful for triggering file-watching tools."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(map! "C-x C-s" #'save-buffer-always)

(map! "C-z" #'ignore)

;; dumb-jump
;; if you run C-u C-M-g (universal argument), it will run the prompt version of dumb-jump-go
(defun custom-dumb-jump-go (&optional _)
  "Delegates to dumb-jump-go. If given the universal argument, runs it with a prompt"
  (interactive "P")
  ;; If universal argument given, prompt
  (if current-prefix-arg
      (dumb-jump-go-prompt)
    (dumb-jump-go)))

(map!
 ;; Bind our customized dumb jump function
 "C-M-g" #'custom-dumb-jump-go)

(map! :map dumb-jump-mode-map
      ;; unbind dumb-jump-back which i always accidentally hit
      "C-M-p" nil)

;; configure auto formatters
(setq +format-on-save-enabled-modes
      '(not haskell-mode))

(after! smart-tab
  (global-smart-tab-mode t))

;; default to opening a project in dired
;; NOTE: not currently used with vertico
(setq counsel-projectile-switch-project-action
      '(4
        ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
        ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
        ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
        ("D" counsel-projectile-switch-project-action-dired "open project in dired")
        ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
        ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
        ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
        ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
        ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
        ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
        ("C" counsel-projectile-switch-project-action-configure "run project configure command")
        ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
        ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
        ("sg" counsel-projectile-switch-project-action-grep "search project with grep")
        ("si" counsel-projectile-switch-project-action-git-grep "search project with git grep")
        ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
        ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
        ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
        ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
        ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
        ("xv" counsel-projectile-switch-project-action-run-vterm "invoke vterm from project root")
        ("Oc" counsel-projectile-switch-project-action-org-capture "capture into project")
        ("Oa" counsel-projectile-switch-project-action-org-agenda "open project agenda")))

;; company mode is really annoying in these modes
(setq company-global-modes
      '(not org-mode erc-mode message-mode help-mode gud-mode vterm-mode sh-mode))

;; spellchecking only used selectively
(remove-hook 'text-mode-hook #'spell-fu-mode)
(add-hook 'markdown-mode-hook #'spell-fu-mode)

;; suppress annoying TAGS prompt
(setq tags-add-tables nil)

;; configure edwina, a tiling window manager
;; this sets display-buffer to open a new window by default. whatever that means
(setq display-buffer-base-action '(display-buffer-below-selected))
(edwina-mode t)

;; https://github.com/ajgrf/edwina/issues/7#issuecomment-663598457
(defun my/edwina-zoom-and-switch ()
  "Zoom/cycle the selected window to/from master area and then switch to the master window."
  (interactive)
  (if (eq (selected-window) (frame-first-window))
      (edwina-swap-next-window)
    (let ((pane (edwina-pane (selected-window))))
      (edwina-delete-window)
      (edwina-arrange (cons pane (edwina-pane-list)))
      ;; switch to master window
      (select-window (car (edwina--window-list))))))

;; copy edwina's bindings into the doom w workspaces/windows leader prefix.
;; might make sense to have additional super-based bindings for exwm mode? maybe
;; that'll be too confusing.
(map! :map edwina-mode-map
      :leader
      :desc "Rearrange panes" "w r" #'edwina-arrange
      :desc "Move to next window cyclically" "w n" #'edwina-select-next-window
      :desc "Move to previous window cyclically" "w p" #'edwina-select-previous-window
      :desc "Move current window into master area" "w RET" #'my/edwina-zoom-and-switch
      :desc "Clone selected window" "w c" #'edwina-clone-window
      :desc "Delete selected window" "w k" #'edwina-delete-window
      :desc "Grow master window size" "w l" #'edwina-inc-mfact
      :desc "Grow master window size" "w h" #'edwina-dec-mfact
      )



;; Disambiguate auth sources
(setq auth-sources '("~/.authinfo.gpg"))

;; Save bookmarks file on every 1 modification. This is useful because with
;; EXWM, emacs is rarely killed
(setq bookmark-save-flag 1)
;; Set an explicit bookmark pack that can be symlinked in from Dropbox on systems that support it
(setq bookmark-default-file "~/.emacs-bookmarks")


;; Open dired when switching projects
(setq projectile-switch-project-action 'projectile-dired)
;;TODO: launch linux app not showing items in PATH
;;TODO: how well will this work in tramp?

;; Allow you to descend into a directory with slash
(map! :map vertico-map
      "/" #'vertico-directory-enter)

;; Consult starts previewing as you type which causes weird stuff to happen on windows like terminal that are in char mode and capture input.
;; Solution is from https://github.com/minad/consult/issues/233#issuecomment-1109006627
(after! consult
  (consult-customize consult-buffer :preview-key nil))
;;TODO add after! counsel with linux launch


;; Taken and slightly modified from  https://codeberg.org/dalz/dotfiles/src/branch/master/doom/+exwm.el
(defun my/app-launcher ()
  "An app launcher that completes using the executables in PATH (labeled with x) and desktop apps (labeled with d)"
  (interactive)
  (ivy-read "run: "
            (append (mapcar (lambda (c)
                              (let* ((name (caar c))
                                     (cats (cdar c))
                                     (str (concat name cats)))
                                (put-text-property 0 (length str) 'display (concat "d: " name) str)
                                (cons str (cdr c))))
                            (my/desktop-apps))
                    (mapcar (lambda (s)
                              (put-text-property 0 (length s) 'display (concat "x: " s) s) s)
                            (my/executables-in-path)))
            :action (lambda (x)
                      (if (consp x)
                          (counsel-linux-app-action-default x)
                        (start-process-shell-command x nil x)))))
(defun my/executables-in-path ()
  (delete-dups
   (mapcar #'f-filename
           (-filter #'f-executable-p
                    (-flatten
                     (mapcar (lambda (dir)
                               (directory-files dir t directory-files-no-dot-files-regexp))
                             (-filter #'f-directory-p (split-string (getenv "PATH") ":"))))))))

(defun my/desktop-apps ()
  (mapcar (lambda (c)
            (cons (with-temp-buffer
                    (insert-file-contents (cdr c))
                    (cons
                     (progn (re-search-forward "^Name *= *\\(.+\\)$")
                            (match-string 1))
                     (progn (goto-char (point-min))
                            (re-search-forward "^Categories *= *\\(.+\\)$" nil t)
                            (match-string 1))))
                  (car c)))
          (counsel-linux-apps-list-desktop-files)))

;;NOTE: I'm under the impression that using require is discouraged becase it slows load time? I can't seem to force counsel to load though so require it is
(require 'counsel)
;;TODO is there a vertico/consult for tramp?

;; By default consult logs to an invisible buffer. This makes debugging a bit easier
(setq consult--async-log "*consult-async*")

;; Slow down how quickly consult searches re-search.
(setq consult-async-input-debounce 0.7)

;;TODO: dumb jump doesn't let you Narrow now?. maybe we should use +lookup/definition for the binding instead of dumb jump?
;; evidently dumb-jump-go isn't even a thing now. you should use sref-find-definitions?
