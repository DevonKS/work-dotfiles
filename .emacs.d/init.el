(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider
                      undo-tree
                      key-chord
                      evil-terminal-cursor-changer
                      use-package
                      company
                      rainbow-delimiters
                      gruvbox-theme
                      material-theme
                      avy
                      spaceline
                      exec-path-from-shell
                      flycheck
                      flycheck-clojure
                      flycheck-pos-tip
                      powerline
                      airline-themes))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(require 'better-defaults)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;; Standard Setup ;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(require 'evil-leader)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-mode 1)
(evil-leader/set-key
  "o" 'find-file
  "e" 'eval-expression
  "r" 'execute-extended-command
  "j" 'avy-goto-char
  "ls" (lambda () (interactive) (split-window-below) (windmove-down) (window-resize nil -15) (term "/bin/bash"))
  "sb" 'ido-switch-buffer
  "bk" 'ido-kill-buffer
  "fs" 'save-buffer
  "sv" 'split-window-below
  "sh" 'split-window-right
  "wh" 'windmove-left
  "wj" 'windmove-down
  "wk" 'windmove-up
  "wl" 'windmove-right
  "wc" 'delete-window
  "wm" 'delete-other-windows
  "ce" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "cr" (lambda () (interactive) (load-file user-init-file)))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)

(unless (display-graphic-p)
      (use-package evil-terminal-cursor-changer
        :ensure t
        :init
        (setq evil-motion-state-cursor 'box)  ; █
        (setq evil-visual-state-cursor 'box)  ; █
        (setq evil-normal-state-cursor 'box)  ; █
        (setq evil-insert-state-cursor 'bar)  ; ⎸
        (setq evil-emacs-state-cursor  'hbar) ; _
        :config
        (etcc-on)
        ))

;I'm disabling these keys so I can use the buildin behavior of C-r (fzf search history) when in term mode.
(define-key evil-insert-state-map (kbd "C-r") nil)
(global-unset-key (kbd "C-r"))

;;;;; Company Mode
(global-company-mode)

;;;;; Rainbow Delimiters Mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
; I don't want the highlighting of matching parens cause I have rainbow delimiters
(show-paren-mode 0)

;;;;; Highlight Symbol
(require 'highlight-symbol)
(evil-leader/set-key "hs" 'highlight-symbol)

;;;;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-check-syntax-automatically '(mode-enabled save))

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;;;;;;;;;;;;;;;;;;;;;;;; Theme Setup ;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'material t)

;(require 'spaceline)
;(require 'spaceline-config)
;(spaceline-spacemacs-theme)
;(setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

(require 'airline-themes)
(load-theme 'airline-doom-one t)

;;;;;;;;;;;;;;;;;;;;;;;; Clojure Setup ;;;;;;;;;;;;;;;;;;;;;;;;
(evil-leader/set-key-for-mode 'clojure-mode
  "mc" 'cider-load-buffer
  "mj" 'cider-jack-in
  "mtc" 'cider-test-run-test
  "mtn" 'cider-test-run-ns-tests
  "mta" 'cider-test-run-project-tests
  "mq" 'cider-quit
  "mrnc" 'cider-repl-set-ns
  "mrg" (lambda () (interactive) (cider-switch-to-repl-buffer t))
  "mrr" 'cider-ns-refresh
  "mgf" '(lambda () (interactive) (cider-find-var t))
  "mgb" 'cider-pop-back
  "mf" 'cider-format-buffer)

(eval-after-load 'cider '(flycheck-clojure-setup))


;TODO
;workspaces

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "9b1c580339183a8661a84f5864a6c363260c80136bd20ac9f00d7e1d662e936a" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" default)))
 '(package-selected-packages
   (quote
    (undo-tree cider clojure-mode projectile better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
