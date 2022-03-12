;;; exwm.el -*- lexical-binding: t; -*-
(require 'exwm-config)
;;(require 'exwm-systemtray) TODO: we'll let mate handle that
;; multi-monitor support
(require 'exwm-randr)

;;TODO: how do we make this conditional?
;; Emacs as a window manager
(exwm-config-default) ;;TODO: get a full config TODO will it work without?
;;(exwm-systemtray-enable)
(exwm-randr-enable)

;; Number workspaces to monitors. this is very specifi to my home setup so if i
;; need to plug in somewhere else it's probably gonna suck. I think I only need
;; a workspace per monitor i'm using though
(setq exwm-randr-workspace-monitor-plist
      (let
          ((dell-monitor "DP-3")
           (laptop-monitor "eDP-1"))
        `(1 ,dell-monitor 2 ,laptop-monitor)))


;; this always has to come last evidently
;;
(exwm-enable) ;; this is "harmless"
;; but logs an annoying warning on boot when exwm is not enabled

;;TODO: how do we get this to work when x windows are in focus?
;;TODO: capture bindings for window movement that overrides x, e.g. C-S-n and C-S p
;;TODO; bind xmonad like bindings for s-w and s-e which runs something like (exwm-workspace-switch-create 2)
(after! counsel
  (map!
        ;; start a program with super-p
        "s-p" #'counsel-linux-app))
