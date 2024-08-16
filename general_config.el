;; custom keymapping
(map!
 "C-w" #'backward-kill-word
 "C-s" #'consult-line
 ;; NOTE: meow can run this with SPC x
 "C-x C-m" #'execute-extended-command
 "C-x C-k" #'kill-region
 "C-S-k" #'kill-whole-line
 "C-;" #'iedit-mode
 "C-/" #'undo-fu-only-undo
 "C-?" #'undo-fu-only-redo
 ;; unbind mail compose
 "C-x m" nil
 ;; This allows me to use the same buffer switching command everywhere,
 ;; including with EXWM and even in char mode
 "s-b" #'switch-to-buffer
 ;; To make sure I get used to s-b, I'll unbind C-x b
 "C-x b" nil
 ;; To make sure I get used to C-c w x and C-c w y, unbind C-x 1, C-x 2, and C-x 3
 "C-x 1" nil
 "C-x 2" nil
 "C-x 3" nil
 )

;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'bu 'browse-url)
(defalias 'ft 'find-tag)

;; Something seems to need the embark map so we'll just force it to load
(use-package! embark :demand t)

(map! :map goto-map
      ;; This translates to M-g c
      :desc "Avy char" "c" #'avy-goto-char)

(after! smartparens
  ;; https://github.com/Fuco1/smartparens/wiki/Example-configuration
  (map! :map smartparens-mode-map
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
        "C-+" #'default-text-scale-increase
        "C--" #'default-text-scale-decrease
        "C-0" #'default-text-scale-reset
        )
  (map! :map emacs-lisp-mode-map
        ")" #'sp-up-sexp)
  (defun my/interactively-wrap-with-pair (pair &optional arg)
    "Like sp-wrap-pair but prompts for the pair as a character"
    (interactive "sPair:")
    (sp-wrap-with-pair pair))

  (sp-with-modes 'org-mode
    ;; "Verbatim"?
    (sp-local-pair "=" "=")
    ;; code
    (sp-local-pair "~" "~")
    ;; italic. Common enough that we don't want it auto-inserting the closing slash
    (sp-local-pair "/" "/" :actions '(wrap))
    ;; bold
    (sp-local-pair "*" "*")
    ;; Strikethrough
    (sp-local-pair "+" "+")
    ;; Underline
    (sp-local-pair "_" "_")
    ;;TODO: can we add mode-specific objects in meow?
    )
  )

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


(after! dumb-jump
  ;; the default is platform-dependent. ripgrep seems very reliable
  (setq dumb-jump-force-searcher 'rg)
  ;; Completing read lets you further filter wich helps deal with the really
  ;; dumb and poorly ordered results that it returns
  (setq dumb-jump-selector 'completing-read)
  )

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
;; TODO: drop?
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

(add-hook! 'markdown-mode-hook #'spell-fu-mode)

;; suppress annoying TAGS prompt
(setq tags-add-tables nil)

;; Disambiguate auth sources
(setq auth-sources '("~/.authinfo.gpg"))

;; Save bookmarks file on every 1 modification. This is useful because with
;; EXWM, emacs is rarely killed
(setq bookmark-save-flag 1)
;; Set an explicit bookmark pack that can be symlinked in from Dropbox on systems that support it
(setq bookmark-default-file "~/.emacs-bookmarks")


;; Open dired when switching projects
(setq projectile-switch-project-action 'projectile-dired)

;; Allow you to descend into a directory with slash
(map! :map vertico-map
      "/" #'vertico-directory-enter)

;; Consult starts previewing as you type which causes weird stuff to happen on windows like terminal that are in char mode and capture input.
;; Solution is from https://github.com/minad/consult/issues/233#issuecomment-1109006627
(after! consult
  (consult-customize consult-buffer :preview-key nil))

(use-package! counsel
  :config
  ;; Taken and slightly modified from  https://codeberg.org/dalz/dotfiles/src/branch/master/doom/+exwm.el
  (defun my/app-launcher ()
    "An app launcher that completes using the executables in PATH (labeled with x) and desktop apps (labeled with d)"
    (interactive)
    (let* ((desktop-apps (mapcar (lambda (c)
                                   (let* ((name (caar c))
                                          (cats (cdar c))
                                          (str (concat name cats)))
                                     (put-text-property 0 (length str) 'display (concat "d: " name) str)
                                     (cons str (cdr c))))
                                 (my/desktop-apps)))
           (executables (mapcar (lambda (s)
                                  (put-text-property 0 (length s) 'display (concat "x: " s) s) s)
                                (my/executables-in-path)))
           (candidates (append desktop-apps executables))
           (selection (completing-read "Run: " candidates))
           )
      (if (consp selection)
          (counsel-linux-app-action-default selection)
        (start-process-shell-command selection nil selection))
      )
    )
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
  )

;;NOTE: I'm under the impression that using require is discouraged becase it slows load time? I can't seem to force counsel to load though so require it is
(after! consult
  ;; By default consult logs to an invisible buffer. This makes debugging a bit easier
  (setq consult--async-log "*consult-async*")
  ;; Slow down how quickly consult searches re-search.
  (setq consult-async-input-debounce 0.7)
  )

(use-package! counsel-tramp
  :config
  (defun tramp-find-file ()
    "Prompt for a TRAMP path from your SSH config and then browse that host"
    (interactive)
    (let* ((host (completing-read "Host: " (counsel-tramp--candidates)))
           (default-directory host))
      (find-file)
      )
    )
  ;; C-c f t to browse files via tramp, replacing counsel-tramp
  (map! :map doom-leader-file-map
        :desc "Find file via TRAMP" "t" #'tramp-find-file)

  )

;; Add some online search providers which can be used with C-c s O
(dolist
    (provider
     '(("Hackage" "https://hackage.haskell.org/packages/search?terms=%s")
       ("Hoogle" "https://hoogle.haskell.org/?scope=set:stackage&hoogle=%s")
       ("JustWatch" "https://www.justwatch.com/us/search?q=%s")
       ;;TODO: wiki search, local hoogle
       ))
  (add-to-list '+lookup-provider-url-alist provider))

(defun mx/well/jira-url (ticket-number)
  "Returns a URL string for the given Jira ticket number"
  (s-lex-format "https://wellco.atlassian.net/browse/${ticket-number}"))

(defun mx/well/browse-jira-ticket (ticket-number)
  "Open the Jira ticket with the given ticket number"
  (interactive "sTicket Number: ")
  (browse-url (mx/well/jira-url ticket-number))
  )

(defun mx/well/memsource-project (project-id)
  "Open the Memsource project with the given project id"
  (interactive (list
                (read-string (format "Project ID (%s): " (thing-at-point 'sexp))
                             nil nil (thing-at-point 'sexp))))
  (browse-url (s-lex-format "https://cloud.memsource.com/web/project2/show/${project-id}"))
  )

;; Add some browse shortcuts
(map! :leader
      (:desc "browse" :prefix ("b" . "browse")
       :desc "Browse URL" "b" #'browse-url
       :desc "Browse Well Jira ticket" "j" #'mx/well/browse-jira-ticket
       :desc "Browse Memsource project" "m" #'mx/well/browse-memsource-project
       ))

;; Add some toggle shortcuts
(map! :leader
      (:desc "toggle" :prefix ("t" . "toggle")
       :desc "Auto-formatting" "a" #'apheleia-mode
       ))

;; Add a few modes that should auto-color hex color codes
(defun mx/enable-rainbow-mode ()
  (rainbow-mode 1))

(add-hook! '(json-mode-hook haskell-mode-hook) 'mx/enable-rainbow-mode)

;; Show a number by company completions, e.g. 1 can be selected with M-1. Unclear if this is going to actually be useful
(setq company-show-quick-access 'left)


;; Set common time zones that i use
(use-package! tzc
  :init
  ;; tzc hardcodes this on linux and it's incorrect on nixos. This has to happen
  ;; before the package is loaded or it will eat shit.
  (let ((tzdir (getenv "TZDIR")))
    (when (file-directory-p tzdir)
      (setq tzc-main-dir (file-name-as-directory (getenv "TZDIR"))))
    )

  :config
  (setq tzc-favourite-time-zones-alist
        '(("America/Los_Angeles" "US_Pacific")
          ("UTC+0000" "UTC")
          ("America/Chicago" "US_Central")
          ("America/New_York" "US_Eastern")
          ("Europe/Istanbul" "Turkey")
          ("Europe/Berlin" "Germany")
          ))
  (map! :leader
        (:desc "time zones" :prefix ("z" . "time zones")
         :desc "World clock" "w" #'tzc-world-clock
         ))
  )

;; Dired is a little too aggressive with hiding
(setq dired-omit-extensions
      '(".hi"
        ".o"
        "~"
        ".lbin"
        ".so"
        ".a"
        ".ln"
        ".blg"
        ".bbl"
        ".elc"
        ".lof"
        ".glo"
        ".idx"
        ".lot"
        ".svn/"
        ".hg/"
        ".git/"
        ".bzr/"
        "CVS/"
        "_darcs/"
        "_MTN/"
        ".fmt"
        ".tfm"
        ".class"
        ".fas"
        ".lib"
        ".mem"
        ".x86f"
        ".sparcf"
        ".dfsl"
        ".pfsl"
        ".d64fsl"
        ".p64fsl"
        ".lx64fsl"
        ".lx32fsl"
        ".dx64fsl"
        ".dx32fsl"
        ".fx64fsl"
        ".fx32fsl"
        ".sx64fsl"
        ".sx32fsl"
        ".wx64fsl"
        ".wx32fsl"
        ".fasl"
        ".ufsl"
        ".fsl"
        ".dxl"
        ".lo"
        ".la"
        ".gmo"
        ".mo"
        ".toc"
        ".aux"
        ".cp"
        ".fn"
        ".ky"
        ".pg"
        ".tp"
        ".vr"
        ".cps"
        ".fns"
        ".kys"
        ".pgs"
        ".tps"
        ".vrs"
        ".pyc"
        ".pyo"
        ".idx"
        ".lof"
        ".lot"
        ".glo"
        ".blg"
        ".bbl"
        ".cp"
        ".cps"
        ".fn"
        ".fns"
        ".ky"
        ".kys"
        ".pg"
        ".pgs"
        ".tp"
        ".tps"
        ".vr"
        ".vrs"))

;; modal editing with meow. Can practice with meow-tutor
(use-package! meow
  :init
  ;; meow wants C-k to be kill-region but we've overridden that to C-x C-k
  (setq meow--kbd-kill-region "C-x C-k")
  (setq meow--kbd-kill-whole-line "C-S-k")

  ;; https://github.com/meow-edit/meow/issues/382#issuecomment-1353971582
  ;;
  ;; I find using negative to reverse direction kind of a pain. Instead, this
  ;; defines a macro that you can use to bind specific reverse versions of things
  ;; like find, till, and search
  (defmacro my--call-negative (form)
    `(let ((current-prefix-arg -1))
       (call-interactively ,form)))

  (defun my-negative-meow-find ()
    "Find a character in the line in reverse."
    (interactive)
    (my--call-negative 'meow-find))

  (defun my-negative-meow-till ()
    "Till a character (i.e. stopping one character shy) in reverse"
    (interactive)
    (my--call-negative 'meow-till))

  (defun my-negative-meow-search ()
    "Reverse the direction of meow-search"
    (interactive)
    (my--call-negative 'meow-search))

  (defun my-meow-redo ()
    "Cancel current selection then redo."
    (interactive)
    (when (region-active-p)
      (meow--cancel-selection))
    (meow--execute-kbd-macro "C-?"))


  ;; Taken from https://github.com/meow-edit/meow/blob/master/KEYBINDING_QWERTY.org
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    ;; https://github.com/meow-edit/meow/issues/111#issuecomment-990785463
    ;; Define angle brackets as a thing you can select with meow
    (meow-thing-register 'angle '(pair ("<") (">")) '(pair ("<") (">")))
    (meow-thing-register 'url 'url 'url)
    (meow-thing-register 'uuid 'uuid 'uuid)

    ;; These will be enabled for all modes
    (add-to-list 'meow-char-thing-table '(?a . angle))
    (add-to-list 'meow-char-thing-table '(?u . url))
    (add-to-list 'meow-char-thing-table '(?U . uuid))

    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     ;; This doesn't seem to be advertised very much but it does a change but
     ;; first kills the code you're replacing so that it's in the kill-ring and
     ;; can be pasted somewhere else. It is useful for extracting bindings.
     '("C" . meow-change-save)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . my-negative-meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     ;; This one works a bit differently than the other negatives. You'd press N
     ;; to reverse and then n to go in that direction
     '("N" . my-negative-meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     ;; Wrap a selection with a given pair, using smartparens
     '("P" . my/interactively-wrap-with-pair)
     ;; Normally this is set to meow-quit which just closes the buffer. I don't really need a super fast way to do that so i've set it to regex replace
     '("q" . meow-query-replace-regexp)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("T" . my-negative-meow-till)
     '("u" . meow-undo)
     ;; Previously this was meow-undo-in-selection but I don't really see how
     ;; that's supposed to be useful. May require additional registration of pairs
     ;; for certain modes.
     '("U" . my-meow-redo)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  :config
  (meow-setup)
  ;; Expand the numbered expand helpers to make them easier to use
  (setq meow-expand-hint-remove-delay 3.0)
  ;; start git commits in insert mode
  (add-hook 'git-commit-mode-hook 'meow-insert-mode)
  ;; Completion candidates should really only be active when inserting. There
  ;; may be a better way to do this. When I exit insert mode, I'm no longer
  ;; trying to modify the text under cursor so the recommendations should go
  ;; away.
  (add-hook 'meow-insert-exit-hook 'corfu-quit)

  ;; Define a dummy state. This will disable the space leader. Not sure if I want that
  (meow-define-state disable "dummy state")

  ;; Defult meow to insert in vterm. I think I'd probably like to totally
  ;; disable meow mode in vterm but I don't know how yet.
  (map-put! meow-mode-state-list 'vterm-mode 'insert)
  (meow-global-mode 1)
  )

;; Commands seem to be failing and when you bring up the transient buffer it
;; shifts focus to the next window and you're not able to issue commands. This
;; is less usable because it shows a terse one-line summary of the magit commands but seems to work
;; (setq transient-show-popup nil)
;; Seems like the issue might be fixed as of 4/16/2023?
(setq transient-show-popup t)

(use-package! plantuml-mode
  :mode "\\.plantuml\\'"
  :config
  ;; If we don't set this, it hits the server and sends it your diagram data!
  ;; On nix I've installed plantuml via the plantuml-c4 package
  (setq plantuml-default-exec-mode 'executable)
  ;; Same thing but for org-babel
  (setq org-plantuml-exec-mode 'plantuml)
  ;; The executable wrapper nix gives puts an argument at the front. By default
  ;; the executable args tries to pass -headless which i think is a java
  ;; argument and must come first. Thus, the output always has a warning about
  ;; that argument and doesn't parse properly. It still seems to work without
  ;; using headless so we'll just not send that.
  (setq plantuml-executable-args nil)
  ;; Same thing but for org-babel
  (setq org-plantuml-args nil)
  ;; Register to use with org mode. This seems to just get syntax highlighting?
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  ;; Register with babel so we can get execution as well
  )

;; This is probably overly aggrssive but eldoc mode is usually insanely annoying
;; when LSP is enabled but not in use. All eldoc is is a way to see the
;; highlighted function's arguments in the minibuffer.
(setq global-eldoc-mode nil)

;; Suppress an annoying warning that comes up on every file of a certain type
;; when there are no supported LSP clients
(setq lsp-warn-no-matched-clients nil)

;; Suppress annoying file watching prompt that comes up frequently
(setq lsp-enable-file-watchers nil)

;; I very rarely if ever want a warning buffer to come up and steal focus. lsp
;; integrations seem to do this a ton.
(setq warning-minimum-level :error)

;; TODO: reenable cape-dabbrev-check-other-buffers?
;; TODO: is all of this still needed?
;; (use-package! cape
;;   :init
;;   (require 'dash)
;;   (require 'cape-keyword)
;;   (defun my/register-capfs (capfs)
;;     "Register the given list of capfs in the current mode."
;;     (-each-r capfs (lambda (capfs) (add-to-list 'completion-at-point-functions capfs)))
;;     )

;;   ;; https://github.com/kenranunderscore/dotfiles/blob/main/home-manager-modules/emacs/emacs.d/config.org#more-completion-at-point-backends-via-cape
;;   (defun my/register-default-capfs ()
;;     "I use these caps everywhere as they are generally useful. This
;; function needs to be called in certain mode hooks, as some modes
;; fill the buffer-local capfs with exclusive completion functions,
;; so that the global ones don't get called at all."
;;     ;; I think we want to see all results from some one of these completions.
;;     ;; I've noticed that individually, one of the capes will start matching and
;;     ;; will suppress results from the others.
;;     (my/register-capfs (list (cape-capf-super #'cape-dabbrev #'cape-keyword) #'cape-file))
;;     )

;;   (my/register-default-capfs)


;;   :hook ((emacs-lisp-mode . (lambda () (my/register-capfs (list (cape-capf-super #'cape-elisp-symbol #'cape-dabbrev #'cape-keyword) #'cape-file))))
;;          (haskell-mode . my/register-default-capfs)
;;          (python-mode . my/register-default-capfs)
;;          )

;;   :config
;;   ;;TODO: i'm not sure why but this length isn't for the overall completion but
;;   ;;seems to be for the substring prefix before case switches, at least with
;;   ;;haskell-mode's completion. Thus in all my testing when I used FooBar as a
;;   ;;completion test, the default of 4 would fail because you get the 3 letters
;;   ;;F-o-o before B acts as a cutoff. I noticed that anything at 3 or below was
;;   ;;completing correctly. We can probably figure out what case setting is
;;   ;;screwing this up eventually but setting this number lower at least works.
;;   (setq cape-dabbrev-min-length 2)

;;   ;; Add some keyword lists to cape keyword completion. I should probably add a
;;   ;; PR but I don't want to deal with the contributor license.
;;   (add-to-list 'cape-keyword-list '(haskell-mode
;;                                     "as"
;;                                     "case"
;;                                     "class"
;;                                     "data family"
;;                                     "data instance"
;;                                     "data"
;;                                     "default"
;;                                     "deriving instance"
;;                                     "deriving"
;;                                     "do"
;;                                     "else"
;;                                     "family"
;;                                     "forall"
;;                                     "foreign import"
;;                                     "foreign"
;;                                     "hiding"
;;                                     "if"
;;                                     "import qualified"
;;                                     "import"
;;                                     "in"
;;                                     "infix"
;;                                     "infixl"
;;                                     "infixr"
;;                                     "instance"
;;                                     "let"
;;                                     "mdo"
;;                                     "module"
;;                                     "newtype"
;;                                     "of"
;;                                     "proc"
;;                                     "qualified"
;;                                     "rec"
;;                                     "signature"
;;                                     "then"
;;                                     "type family"
;;                                     "type instance"
;;                                     "type"
;;                                     "where")
;;                )


;;   )

;; Edwina is a window manager in emacs and it becomes my tiling window manager
;; under EXWM.

;; some code taken from.
;; https://github.com/lucasgruss/.doom.d/blob/master/config.org#edwina I'm not
;; exactly sure what this does but I've noticed less weird window focusing
;; issues and I so far seem to be able to use transient-based menus like magit
;; and docker again.
(use-package! edwina
  :init
  (defvar my/display-buffer-alist-save nil
    "Stores previous value of display-buffer-alist before entering edwina-mode")
  (defvar my/display-buffer-base-action-save nil
    "Stores previous value of display-buffer-base-action before entering edwina-mode")
  (defun my/edwina-remove-or-restore-buffer-behavior ()
    "If edwina-mode is active, save and set to nil the following variables:

       - display-buffer-alist
       - display-buffer-base-action

 or restore it if edwina mode is inactive."
    (if edwina-mode
        (progn
          (setq my/display-buffer-base-action-save display-buffer-base-action)
          (setq display-buffer-base-action '(display-buffer-below-selected))
          (setq my/display-buffer-alist-save display-buffer-alist)
          (setq display-buffer-alist nil))
      (setq display-buffer-base-action my/display-buffer-base-action-save)
      (setq display-buffer-alist my/display-buffer-alist-save)))

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

  :hook (edwina-mode . my/edwina-remove-or-restore-buffer-behavior)
  :bind (("C-S-p" . edwina-select-previous-window)
         ("C-S-n" . edwina-select-next-window)
         )
  :config
  (map! :map edwina-mode-map
        :leader
        :desc "Rearrange panes" "w r" #'edwina-arrange
        :desc "Move to next window cyclically" "w n" #'edwina-select-next-window
        :desc "Move to previous window cyclically" "w p" #'edwina-select-previous-window
        :desc "Move current window into master area" "w RET" #'my/edwina-zoom-and-switch
        :desc "Clone selected window" "w c" #'edwina-clone-window
        :desc "Delete selected window" "w k" #'edwina-delete-window
        :desc "Grow master window size" "w l" #'edwina-inc-mfact
        :desc "Grow master window size" "w h" #'edwina-dec-mfact)
  (edwina-mode t)
  )

;; Technically these aren't particular to edwina mode and should be bound
;; regardless because I've unbound the alternative bindings
(map! :leader
      ;; These are not edwina functions but they can be used in a window with char mode on
      :desc "Delete other windows except the focused one" "w 1" #'delete-other-windows
      :desc "Horizontally split window to the right" "w x" #'split-window-right
      :desc "Vertically split window to the bottom" "w y" #'split-window-below)

(after! pdf-tools
  :init
  ;; Build an epdfinfo executable needed by the progrma. Haven't figured out the nixy way to do this.
  (pdf-tools-install)
  ;; https://github.com/Fuco1/smartparens/wiki/Example-configuration
  (map! :map pdf-view-mode-map
        ;; Meow bindings
        "j" #'pdf-view-next-page-command
        "k" #'pdf-view-previous-page-command
        ;; pdf-view mode hooks into isearch system so we need to set up those
        ;; bindings to override, otherwise it'll search through the literal
        ;; source code of the PDF
        "C-s" #'isearch-forward
        "C-r" #'isearch-backward
        )
  )

(use-package! dired-rsync-transient
  :after dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-x" . dired-rsync-transient))
  )

(use-package! lab
  :config
  (setq lab-host "https://gitlab.com")
  (setq my/well-gitlab-group "4487284")
  (setq my/soostone-gitlab-group "4206464")
  ;; Default group operations to well
  (setq lab-group my/well-gitlab-group)
  )

(use-package! pushover
  :config
  ;; TODO: pushover-api-key and pushover-user-key
  )

(use-package! 1password
  :demand t
  :config
  (1password-auth-source-enable)
  )

(use-package! alert
  :config
  ;; Add an alert style that uses pushover
  (alert-define-style 'pushover :title "Pushover"
                      :notifier
                      (lambda (info)
                        (pushover-send (plist-get info :title) (plist-get info :message))
                        ))
  ;; Send lab alerts to pushover
  (add-to-list 'alert-user-configuration '(((:title . "lab.el")) pushover nil))
  ;; Send Jira alerts to pushover
  (add-to-list 'alert-user-configuration '(((:title . "Jira")) pushover nil))
  )

(defun my/refresh-secrets ()
  "Prompt and then sign into 1password and then set some variables with them."
  (interactive)
  ;; This works on the first signin but it may expire
  (unless 1password--session-token (call-interactively #'1password-manual-signin))
  (setq pushover-user-key (1password-get-field "pushover" "User Key"))
  (setq lab-token (1password-get-field "well gitlab" "dev machine access token"))
  (setq jiralib2-token (1password-get-password "Well Jira Token"))
  (message "Secrets refreshed"))

(setq my/arbiter-project-id 12589740)
(defun my/lab-get-mr (project-id mr-iid)
  "Get a single MR by the project id and the mr iid. If an MR is referenced by !123 then the iid is 123"
  (lab--request (format "projects/%s/merge_requests/%s" project-id mr-iid)))


(defun my/lab-merged-p (project-id mr-iid)
  "Check if an MR is merged yet"
  (let-alist (my/lab-get-mr project-id mr-iid)
    (string-equal .state "merged")
    )
  )

(cl-defun my/watch-condition-then (&key condition then (check-frequency 30) (check-limit nil) gave-up rerun?)
  "Watch the :condition function until it returns t and then run the :then function.

Specify a :check-frequency to indicate how often you want to check. Defaults to 30s
Specify a :check-limit to limit how many times you want to check. Defaults to nil, indicating that there's no limit.
Specify a :gave-up function that will be called if the condition didn't come true before the check-limit"

  (lexical-let ((condition condition)
                (then then)
                (check-frequency check-frequency)
                (check-limit check-limit)
                (gave-up gave-up))

    (run-with-timer (if rerun? check-frequency 1)
                    nil
                    (lambda ()
                      (message "INNER condition %s then %s check-frequency %s check-limit %s" condition then check-frequency check-limit)
                      (if (funcall condition)
                          (funcall then)
                        (let ((new-check-limit (if check-limit
                                                   (- check-limit 1)
                                                 check-limit)))
                          (if (or (not check-limit)
                                  (> check-limit 0))
                              (my/watch-condition-then
                               :condition condition
                               :then then
                               :check-frequency check-frequency
                               :check-limit new-check-limit
                               :gave-up gave-up
                               :rerun? t)
                            (when gave-up (funcall gave-up))
                            )
                          )))
                    ))
  )

(defun my/lab-watch-mr-merge (project-id mr-iid)
  "Poll an MR to see if it has gone to state = \"merged\". You should probably only use it for MRs that are unlikely to go unmerged indefinitely."
  (message ">> Started watching MR !%s on project %s" mr-iid project-id)
  ;;TODO: unfortuantely i need to do this lexical let so that project-id and mr-iid make it into the lambda's closure
  ;; there's probably a better way but i don't know it right now
  (lexical-let* ((project-id project-id)
                 (mr-iid mr-iid))
    (my/watch-condition-then
     :condition (lambda () (my/lab-merged-p project-id mr-iid))
     :then (lambda () (lab--alert (format "MR !%s on project %s has merged!" mr-iid project-id)))
     :gave-up (lambda () (lab--alert (format "Gave up waiting for MR !%s on project %s." mr-iid project-id)))
     :check-frequency 30
     :check-limit 240 ;; 2 hours
     )
    )
  )

(defun my/lab-list-arbiter-pipelines ()
  "List recent arbiter pipelines so they can be acted upon"
  (interactive)
  (lab-list-project-pipelines my/arbiter-project-id))

(use-package! jiralib2
  :config
  (setq jiralib2-url "https://wellco.atlassian.net")
  (setq jiralib2-auth 'token)
  (setq jiralib2-user-login-name "michael.xavier@well.co"))

(defun my/alist-get-path (alist keys)
  "Runs alist-get successively with the elements of the keys list, descending into a nested alist.
  For example: (my/alist-get-path my-nested-alist '(outermost-field middle-field innermost-field))"
  (-reduce-from (-flip #'alist-get) alist keys))

(defun my/get-jira-issue-status (issue-key)
  "Return the status string for a Jira issue. This may be project specific."
  (my/alist-get-path (jiralib2-get-issue issue-key) '(fields status name)))

(defun my/get-jira-issue-title (issue-key)
  "Return the title of a Jira issue. This may be project specific."
  (my/alist-get-path (jiralib2-get-issue issue-key) '(fields summary)))

(defun my/jira-issue-approved-or-done-p (issue-key)
  "Returns t if the issue is in either the Approved or Done status."
  (let ((status (my/get-jira-issue-status issue-key)))
    (or (string-equal status "Done")
        (string-equal status "Approved"))))

(defun my/jira-alert (msg)
  "Sends a message about Jira through alert."
  (alert msg :title "Jira"))

(defun my/jira-watch-issue-approve (issue-key)
  "Poll a Jira issue and alert when it moves to Approved or Done"
  (message ">> Started watching Jira issue !%s" issue-key)
  ;;TODO: unfortuantely i need to do this lexical let so that project-id and mr-iid make it into the lambda's closure
  ;; there's probably a better way but i don't know it right now
  (lexical-let* ((issue-key issue-key))
    (my/watch-condition-then
     :condition (lambda () (my/jira-issue-approved-or-done-p issue-key))
     :then (lambda () (lab--alert (format "Issue %s was approved!" issue-key)))
     :gave-up (lambda () (lab--alert (format "Gave up waiting on issue %s approval!" issue-key)))
     :check-frequency 30
     :check-limit 240 ;; 2 hours
     )
    )
  )

(use-package! uuidgen
  :demand t
  :config
  ;; Makes it so leader i u inserts a uuid
  (map! :map doom-leader-insert-map
        :desc "UUIDv4" "u" #'uuidgen
        )
  )

(use-package! lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :config
  (setq lsp-nix-nil-formatter ["nixpkgs-fmt"])
  )

;; TODO: is this a reasonable way to set up lsp for nix?
(add-hook! 'nix-mode-hook #'lsp!)

(use-package! daemons
  ;; Lazily load when daemons command is run
  :commands daemons
  :config
  ;; Only manage --user services for now
  (setq daemons-systemd-is-user t)
  )

(defun mx/well/insert-jira-ticket-url (ticket-number)
  "Given a Jira ticket number, insert the url to that ticket at point."
  (interactive "sTicket Number: ")
  (insert (mx/well/jira-url ticket-number))
  )

(defun mx/insert-jira-ticket-title (ticket-number)
  "Given a Jira ticket number, fetch the title and insert it at point."
  (interactive "sTicket Number: ")
  (insert (my/get-jira-issue-title ticket-number))
  )


;; Add some insert shortcuts
(map! :leader
      (:prefix ("i j" . "jira")
       :desc "Well Jira URL" "u" #'mx/well/insert-jira-ticket-url
       :desc "Jira title" "t" #'mx/insert-jira-ticket-title))

(use-package! comby
  :commands comby)

;; This seems to allow tramp to access the PATH on the remote system
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Projectile runs FD to get files and it seems to get polluted with color
;; control characters unless you add --color never
(setq projectile-git-fd-args "-H -0 --color never -E .git -tf --strip-cwd-prefix")

(defun my/insert-unix-timestamp ()
  "Insert the current unix timestamp at point"
  (interactive)
  (insert (format-time-string "%s")))

(map! :map doom-leader-insert-map
      :desc "UNIX timestamp" "t" #'my/insert-unix-timestamp
      )

;; Writing large files seems to go way way faster with scp. counsel-tramp will
;; use tramp-default-method to build the candidates from your ssh file. So if
;; tramp-default-method is set to ssh, it will open those hosts using ssh and
;; then all file saves will use ssh
(setq tramp-default-method "scp")


(defun my/open-terminal ()
  "Add a centralized function which opens a terminal in the current directory, regardless of if you're using EXWM."
  (interactive)
  (+vterm/here t))

(map! "s-t" #'my/open-terminal)

;; Give reasonable buffer names for vterm so they can be distinguished
(setq vterm-buffer-name-string "*vterm* %s")

;; Doom by default turns off modeline for vterm but i find it helps me identify
;; which vterm is focused
(remove-hook! 'vterm-mode-hook #'hide-mode-line-mode)

;; novice.el disables upcase-region and downcase-region for some reason but I use it a lot
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Configure agenix for editing secrets
(use-package! agenix
  ;; Set up direnv integration
  :hook (agenix-pre-mode . envrc-mode)
  )

(use-package! mermaid-mode)
(use-package! ob-mermaid)

(use-package! corfu
  :config
  ;; preselect the first option in the menu so long as it's valid. saves a keystroke if you complete often
  (setq corfu-preselect 'valid))

(use-package! cape
  :init
  (require 'dash)
  (require 'cape-keyword)
  (defun my/register-capfs (capfs)
    "Register the given list of capfs in the current mode."
    (-each-r capfs (lambda (capfs) (add-to-list 'completion-at-point-functions capfs)))
    )

  ;; https://github.com/kenranunderscore/dotfiles/blob/main/home-manager-modules/emacs/emacs.d/config.org#more-completion-at-point-backends-via-cape
  (defun my/register-default-capfs ()
    "I use these caps everywhere as they are generally useful. This
function needs to be called in certain mode hooks, as some modes
fill the buffer-local capfs with exclusive completion functions,
so that the global ones don't get called at all."
    ;; I think we want to see all results from some one of these completions.
    ;; I've noticed that individually, one of the capes will start matching and
    ;; will suppress results from the others.
    (my/register-capfs (list (cape-capf-super #'cape-dabbrev #'cape-keyword) #'cape-file))
    )

  (my/register-default-capfs)


  ;; Haskell has shit completion that never works so we'll just fully override it
  :hook ((haskell-mode . my/register-default-capfs)
         (python-mode . my/register-default-capfs)
         )

  :config
  ;;TODO: i'm not sure why but this length isn't for the overall completion but
  ;;seems to be for the substring prefix before case switches, at least with
  ;;haskell-mode's completion. Thus in all my testing when I used FooBar as a
  ;;completion test, the default of 4 would fail because you get the 3 letters
  ;;F-o-o before B acts as a cutoff. I noticed that anything at 3 or below was
  ;;completing correctly. We can probably figure out what case setting is
  ;;screwing this up eventually but setting this number lower at least works.
  ;; (setq cape-dabbrev-min-length 2)
  )
