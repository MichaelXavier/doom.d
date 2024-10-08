;;; exwm.el -*- lexical-binding: t; -*-

;; lots taken from https://github.com/johanwiden/exwm-setup
;; TODO: should these be after!?

;; Multi-monitor support
(use-package! exwm-randr
  :commands (exwm-randr-enable)
  :init
  ;; TODO: can we get rid of the multi monitor file? it's kind of clunky
  (load! "exwm/multi_monitor.el")
  ;; TODO: can we avoid this?
  :demand t
  )

;; When editing any text field in an X program, hit C-c ' to edit the text in an
;; emacs buffer, then C-c to save or C-k to abandon
(use-package! exwm-edit
  :init
  ;; This seems to be a little less disruptive to the window layout
  (setq exwm-edit-split t)
  (defun my/on-exwm-edit-compose ()
    "Runs my customized hooks when exwm composition window appears."
    ;; Enable markdown mode since most longform text is probably markdown entry
    (funcall 'markdown-mode)
    ;; Enable zen mode for a slightly cleaner writing experience
    (+zen/toggle 1)
    )
  :hook (exwm-edit-compose . my/on-exwm-edit-compose)
  ;; TODO: not sure why i have to do this but it's required for exwm and i need this loaded beforehand and adding :after to exwm doesn't seem to help
  :demand t
  )

;;TODO: namespace everything with exwm prefix
(use-package! exwm
  :commands (exwm-enable)
  ;; Counsel needed for my application launcher
  :after (counsel)
  :init
  ;; We put a lot of stuff in init because exwm seems pretty sensitive about things like keybindings being configured before setup
  ;; (defvar exwm-terminal-command
  ;;   "kitty"
  ;;   "The command to run your preferred terminal via s-r")
  (defun exwm-rename-buffer ()
    "Make the buffer name always refresh the class and title name of X windows. See https://github.com/ch11ng/exwm/issues/198#issuecomment-249723369"
    (interactive)
    (exwm-workspace-rename-buffer
     (concat exwm-class-name ":"
             (if (<= (length exwm-title) 50) exwm-title
               (concat (substring exwm-title 0 49) "...")))))

  (defun exwm-launch-terminal ()
    "Function used to launch a terminal from inside exwm"
    (interactive)
    ;; (call-process exwm-terminal-command nil 0 nil)
    (my/open-terminal)
    )

  (defun exwm-focus-left-monitor ()
    "Focus on the left monitor, aka workspace 1"
    (interactive)
    (exwm-workspace-switch-create 1))

  (defun exwm-focus-right-monitor ()
    "Focus on the right monitor, aka workspace 2"
    (interactive)
    (exwm-workspace-switch-create 2))

  (defun exwm-ensure-workspace-exists (frame-or-index)
    "Ensure that the given numbered workspace exists"
    (interactive)
    (unless (or (framep frame-or-index)
                (< frame-or-index (exwm-workspace--count)))
      ;; Grow the workspace count to meet the requested number and fill them with frames
      (let ((exwm-workspace--create-silently t))
        (dotimes (_ (min exwm-workspace-switch-create-limit
                         (1+ (- frame-or-index
                                (exwm-workspace--count)))))
          (make-frame))
        (run-hooks 'exwm-workspace-list-change-hook))))

  (defun exwm-workspace-move-switch-create (workspace)
    "Move the current window to the given workspace number and then switch to that workspace"
    (interactive "n")
    (exwm-ensure-workspace-exists workspace)
    (exwm-workspace-move-window workspace)
    (exwm-workspace-switch workspace))

  (defun exwm-move-window-to-left-monitor ()
    "Move the current window to the left monitor, aka workspace 1"
    (interactive)
    (exwm-workspace-move-switch-create 1))

  (defun exwm-move-window-to-right-monitor ()
    "Move the current window to the right monitor, aka workspace 2"
    (interactive)
    (exwm-workspace-move-switch-create 2))

  ;;TODO: how to get exwm to reload its global keys?

  ;; Set keybindings that work everywhere, including in X windows in both line and char mode
  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset (to line-mode).
          (,(kbd "s-r") . exwm-reset)
          ;; Pretty much all of the time we're on 1 or 2 monitors. I prefer to
          ;; just have one workspace per monitor and then use buffer/window
          ;; management to pull up and arrange stuff
          ;;
          ;; XMonad-like bindings: s-w shifts to the "left" workspace/monitor 1,
          ;; and s-e to the "right" workspace/monitor 2
          (,(kbd "s-w") . exwm-focus-left-monitor)
          (,(kbd "s-W") . exwm-move-window-to-left-monitor)
          (,(kbd "s-e") . exwm-focus-right-monitor)
          (,(kbd "s-E") . exwm-move-window-to-right-monitor)
          ;; 's-p': Launch application.
          (,(kbd "s-p") . my/app-launcher)

          ;; Toggle between "line-mode" and "char mode"
          (,(kbd "s-i") . exwm-input-toggle-keyboard)
          (,(kbd "C-S-p") . edwina-select-previous-window)
          (,(kbd "C-S-n") . edwina-select-next-window)

          (,(kbd "s-t") . exwm-launch-terminal)

          ;; I'd prefer to use C-x b to switch buffers but that's problematic to
          ;; enable in char mode so I'm going to try to get used to using this
          (,(kbd "s-b") . switch-to-buffer)

          ;; 's-N': Switch to a certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; 'S-s-N': Move window to, and switch to, a certain workspace.
          ,@(cl-mapcar (lambda (c n)
                         `(,(kbd (format "s-%c" c)) .
                           (lambda ()
                             (interactive)
                             (exwm-workspace-move-switch-create ,n))))
                       '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
                       ;; '(?\= ?! ?\" ?# ?¤ ?% ?& ?/ ?\( ?\))
                       (number-sequence 0 9))
          ))

  ;; passthrough keys when using X programs in line mode. If you switch to char
  ;; mode with s-i, there is no simulation, everything goes straight through to
  ;; the X program. the first element is the key that you input and the second is
  ;; the key that is sent
  (setq exwm-input-simulation-keys
        `((,(kbd "C-b") . ,(kbd "<left>"))
          (,(kbd "C-f") . ,(kbd "<right>"))
          (,(kbd "C-p") . ,(kbd "<up>"))
          (,(kbd "C-n") . ,(kbd "<down>"))
          (,(kbd "C-a") . ,(kbd "<home>"))
          (,(kbd "S-M-,") . ,(kbd "<home>"))
          (,(kbd "C-e") . ,(kbd "<end>"))
          (,(kbd "S-M-.") . ,(kbd "<end>"))
          (,(kbd "M-v") . ,(kbd "<prior>"))
          (,(kbd "C-v") . ,(kbd "<next>"))
          (,(kbd "C-d") . ,(kbd "<delete>"))
          (,(kbd "C-k") . ,(kbd "S-<end> <delete>"))
          ;; kill-ring-save is analogous to copy in most GUI apps
          (,(kbd "M-w") . ,(kbd "C-c"))
          ;; Yank is analogous to paste in most GUI apps
          (,(kbd "C-y") . ,(kbd "C-v"))
          ))

  ;; Override default behaviors for some windows
  (setq exwm-manage-configurations
        '(
          ;; applications which have too much hotkey overlap with exwm should start in char-mode
          ((member exwm-class-name '("Alacritty" "kitty" "Blender")) char-mode t)
          ))

  ;; suggested in ;;https://github.com/ch11ng/exwm/issues/50#issuecomment-140746653
  ;; Show buffers from all workspaces in the buffer list
  (setq exwm-workspace-show-all-buffers t)
  ;; Undocumented. Allows moving an X window by switching to its buffer
  (setq exwm-layout-show-all-buffers t)

  :hook (
         ;; Rename buffers when class/title renames
         (exwm-update-class . exwm-rename-buffer)
         (exwm-update-title . exwm-rename-buffer)
         )

  ;; exwm-edit needs to come first so it can set up bindings since exwm seems to need X bindings declared before loading?
  ;; TODO doesn't seem to work. seems like i still have to put demand t on exwm-edit and exwm-randr and sequence them first
  ;; :after (:all exwm-edit exwm-randr)
  ;; TODO: why doesn't setting :commands to (exwm-enable) force this to load later in the file? why do i have to demand?
  :demand t
  )

(use-package! corfu
  :config
  ;; https://github.com/emacs-exwm/exwm/issues/31#issuecomment-2029704237
  ;; TODO: this kind of works but the suggestions appear quite a bit below the cursor now unless you're near the bottom of the buffer and then they still cover
  (defun get-focused-monitor-geometry ()
    "Get the geometry of the monitor displaying the selected frame in EXWM."
    (let* ((monitor-attrs (frame-monitor-attributes))
           (workarea (assoc 'workarea monitor-attrs))
           (geometry (cdr workarea)))
      (list (nth 0 geometry) ; X
            (nth 1 geometry) ; Y
            (nth 2 geometry) ; Width
            (nth 3 geometry) ; Height
            )))

  (defun advise-corfu-make-frame-with-monitor-awareness (orig-fun frame x y width height buffer)
    "Advise `corfu--make-frame` to be monitor-aware, adjusting X and Y according to the focused monitor."

    ;; Get the geometry of the currently focused monitor
    (let* ((monitor-geometry (get-focused-monitor-geometry))
           (monitor-x (nth 0 monitor-geometry))
           (monitor-y (nth 1 monitor-geometry))
           (selected-frame-position (frame-position))
           (selected-frame-x (car selected-frame-position))
           (selected-frame-y (cdr selected-frame-position))
           (new-x (+ monitor-x selected-frame-x x))
           (new-y (+ monitor-y selected-frame-y y)))

      ;; Call the original function with potentially adjusted coordinates
      (funcall orig-fun frame new-x new-y width height buffer)))

  (advice-add 'corfu--make-frame :around #'advise-corfu-make-frame-with-monitor-awareness))


;; Emacs as a window manager
(exwm-randr-enable)

(exwm-enable) ;; this is "harmless"
;; but logs an annoying warning on boot when exwm is not enabled
