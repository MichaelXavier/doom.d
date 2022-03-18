;;; exwm.el -*- lexical-binding: t; -*-
(require 'exwm-config)
;; multi-monitor support
(require 'exwm-randr)

(load! "exwm/multi_monitor.el")

;;TODO: how do we make this conditional?

;; lots taken from https://github.com/johanwiden/exwm-setup

;; Number workspaces to monitors. this is very specifi to my home setup so if i
;; need to plug in somewhere else it's probably gonna suck. I think I only need
;; a workspace per monitor i'm using though
;; (setq exwm-randr-workspace-monitor-plist
;;       (let
;;           ((dell-monitor "DP-3")
;;            (laptop-monitor "eDP-1"))
;;         `(1 ,dell-monitor 2 ,laptop-monitor)))

;;TODO: shold we do server-
;;TODO: is this making x windows float?

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
   (call-process "alacritty" nil 0 nil))

;; keybindings
(setq exwm-input-global-keys
      `(
        ;; 's-r': Reset (to line-mode).
        (,(kbd "s-r") . exwm-reset)
        ;; 's-w': Switch workspace. TODO: keep?
        (,(kbd "s-w") . exwm-workspace-switch)
        ;; 's-p': Launch application.
        (,(kbd "s-p") . counsel-linux-app)
        ;; Toggle between "line-mode" and "char mode"
        (,(kbd "s-i") . exwm-input-toggle-keyboard)
        ;;TODO: does this make it so they don't get captured in native windows?
        (,(kbd "C-S-p") . edwina-select-previous-window)
        (,(kbd "C-S-n") . edwina-select-next-window)

        (,(kbd "s-t") . exwm-launch-terminal)
        ;;TODO: s-S-<return> to launch terminal
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
        (,(kbd "C-e") . ,(kbd "<end>"))
        (,(kbd "M-v") . ,(kbd "<prior>"))
        (,(kbd "C-v") . ,(kbd "<next>"))
        (,(kbd "C-d") . ,(kbd "<delete>"))
        (,(kbd "C-k") . ,(kbd "S-<end> <delete>"))))

;;TODO: this might help with multiple monitors
;;(setq framemove-hook-into-windmove t)

;;TODO: exwm-firefox?

;; this always has to come last evidently
;;
(exwm-enable) ;; this is "harmless"
;; but logs an annoying warning on boot when exwm is not enabled

;;TODO; bind xmonad like bindings for s-w and s-e which runs something like (exwm-workspace-switch-create 2)
