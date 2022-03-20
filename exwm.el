;;; exwm.el -*- lexical-binding: t; -*-
(require 'exwm-config)
;; multi-monitor support
(require 'exwm-randr)

(load! "exwm/multi_monitor.el")

;; lots taken from https://github.com/johanwiden/exwm-setup

;; Emacs as a window manager
(exwm-randr-enable)

;; make the buffer name always reflect the class and title name of X windows
;; https://github.com/ch11ng/exwm/issues/198#issuecomment-249723369
(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 50) exwm-title
             (concat (substring exwm-title 0 49) "...")))))

;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)


(defun exwm-launch-terminal ()
   "Function used to launch a terminal from inside exwm"
   (interactive)
   (call-process "kitty" nil 0 nil))

;; keybindings
(setq exwm-input-global-keys
      `(
        ;; 's-r': Reset (to line-mode).
        (,(kbd "s-r") . exwm-reset)
        ;; 's-w': Switch workspace.
        (,(kbd "s-w") . exwm-workspace-switch)
        ;; 's-p': Launch application.
        (,(kbd "s-p") . counsel-linux-app)
        ;; Toggle between "line-mode" and "char mode"
        (,(kbd "s-i") . exwm-input-toggle-keyboard)
        (,(kbd "C-S-p") . edwina-select-previous-window)
        (,(kbd "C-S-n") . edwina-select-next-window)

        (,(kbd "s-t") . exwm-launch-terminal)

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
                           (exwm-workspace-move-window ,n)
                           (exwm-workspace-switch ,n))))
                     '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
                     ;; '(?\= ?! ?\" ?# ?¤ ?% ?& ?/ ?\( ?\))
                     (number-sequence 0 9))
        ))

;;TODO: how to get exwm to reload its global keys?

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
        (,(kbd "C-k") . ,(kbd "S-<end> <delete>"))))

;; Override default behaviors for some windows
(setq exwm-manage-configurations
      '(
        ;; terminal epplications have a lot of hotkey overlap with emacs
        ((member exwm-class-name '("Alacritty" "kitty")) char-mode t)
        ))

;; suggested in ;;https://github.com/ch11ng/exwm/issues/50#issuecomment-140746653
;; Show buffers from all workspaces in the buffer list
(setq exwm-workspace-show-all-buffers t)
;; Undocumented. Allows moving an X window by switching to its buffer
(setq exwm-layout-show-all-buffers t)

;; this always has to come last evidently
;;
(exwm-enable) ;; this is "harmless"
;; but logs an annoying warning on boot when exwm is not enabled
