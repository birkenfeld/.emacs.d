;; Setup specially for Macs

(when (equal system-type 'darwin)
  ;; Set up option keys correctly
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)

  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)

  ;; Ignore .DS_Store files with ido mode
  (add-to-list 'ido-ignore-files "\\.DS_Store")

  ;; Keybinding to toggle full screen mode
  (global-set-key (quote [M-f10]) (quote ns-toggle-fullscreen))

  (global-set-key (kbd "s-u") 'universal-argument)
  (global-set-key (kbd "s--") 'negative-argument)

  ;; Mac friendly font
  (when window-system
    (set-face-attribute 'default nil
                        :font "-apple-Monaco-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1"))

  )
