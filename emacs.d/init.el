;;; package --- repo
;;; Commentary:
;;; Personal Emacs init.el
;;; Code:
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; set emacs encoding
(setq current-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Show more info on the title
(setq-default frame-title-format
    (list invocation-name "@" system-name ":%b %+%+ %f"))


(setq inhibit-startup-message t)          ;; Don't show GNU splash screen
(setq-default make-backup-files nil)      ;; No backup file
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(transient-mark-mode t)                   ;; Highlight selected region
(delete-selection-mode 1)
(global-font-lock-mode t)
(menu-bar-mode t)
(tool-bar-mode -1)                        ;; No toolbar
(scroll-bar-mode -1)                      ;; No scroll bar
(global-auto-revert-mode 1)               ;; Refresh a opened file

(setq invisible-bell t)
(setq ring-bell-function (lambda ()))     ;; No bell

(mouse-avoidance-mode 'animate)           ;; Auto mouse avoidance of curosr
(setq mouse-yank-at-point t)              ;; Paste at point not cursor
(setq x-select-enable-clipboard t)

(global-linum-mode t)                     ;; Show line numbers
(column-number-mode t)
(global-hl-line-mode t)                   ;; Highlight current line
(show-paren-mode t)                       ;; Highlight matching parenthesis
;; (global-visual-line-mode t)
(setq line-move-visual nil)
(setq-default indent-tabs-mode nil)       ;; Use spaces instead of tabs
(setq-default tab-width 4)
(setq-default fill-column 80)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-day-and-date t)

;; (setq default-frame-alist
;;  '((top . 25) (left . 0) (height . 32) (width . 80)))

;; Appearance customization
;; (load-theme 'tsdh-light)
;; (set-frame-parameter nil 'font "Droid Sans Mono-11")

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
      (if (equal 'fullboth current-value)
        (if (boundp 'old-fullscreen) old-fullscreen nil)
        (progn (setq old-fullscreen current-value)
          'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)
(global-set-key [f5] 'recompile)
(global-set-key [(meta g)] 'goto-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "RET") 'newline-and-indent)
;; (define-key c-mode-map (kbd "<f6>") 'semantic-ia-describe-class)
;; (define-key c-mode-map (kbd "<f7>") 'semantic-ia-show-doc)
;; (define-key c-mode-map (kbd "<f8>") 'semantic-ia-show-summary)
;; (define-key c++-mode-map (kbd "<f6>") 'semantic-ia-describe-class)
;; (define-key c++-mode-map (kbd "<f7>") 'semantic-ia-show-doc)
;; (define-key c++-mode-map (kbd "<f8>") 'semantic-ia-show-summary)


;; Spell checking
;; (setq ispell-program-name "aspell")
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)


(setq c-default-style "linux")

(setq default-directory "~/Workspace/")

;; Ropemacs
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (pymacs-load "ropemacs" "rope-")
;; (setq repemacs-enable-autoimport t)

(package-initialize)

(add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)

;; ibuffer
(setq ibuffer-expert t)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode t)

;; ido with better flx fuzzy matching
(require 'flx-ido)
(setq ido-use-faces nil)
(flx-ido-mode 1)
(ido-everywhere 1)
(ido-mode t)

(require 'font-lock+)
(require 'dired+)
(which-function-mode t)
(autopair-global-mode t)
(global-flycheck-mode t)
(global-rainbow-delimiters-mode t)

;; pretty mode
(add-hook 'prog-mode-hook 'pretty-symbols-mode)
(add-hook 'python-mode-hook 'pretty-symbols-mode)
(require 'latex-pretty-symbols)

;; auto-complete
(defun c-add-ac-source ()
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
(eval-after-load "auto-complete" '(progn (ac-ispell-setup)))
(require 'auto-complete-config)
(require 'ispell)
(add-to-list 'ac-sources 'ac-source-ispell)
(add-to-list 'ac-sources 'ac-source-yasnippet)
(ac-config-default)
(global-auto-complete-mode t)
(add-hook 'LaTeX-mode-hook 'ac-ispell-ac-setup)
(add-hook 'text-mode-hook 'ac-ispell-ac-setup)
(add-hook 'org-mode-hook 'ac-ispell-ac-setup)
(add-hook 'c-mode-common-hook 'c-add-ac-source)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; Emacs Development Environment
(global-ede-mode 1)
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-summary-mode 1)
;; (speedbar 1)
;; (setq speedbar-use-images nil)

;; org
(org-ac/config-default)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t)
   (dot . t)
   (haskell . t)
   (java . t)
   (perl . t)
   (python . t)
   (scheme . t)
   (R . t)
   (ruby . t)
   ))

(require 'ess-site)

;; Remove trailing white spaces in w3m
(add-hook 'w3m-display-hook
          (lambda (url)
            (let ((buffer-read-only nil))
              (delete-trailing-whitespace))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 3)
 '(doc-view-continuous t)
 '(ess-R-font-lock-keywords (quote ((ess-R-fl-keyword:modifiers . t) (ess-R-fl-keyword:fun-defs . t) (ess-R-fl-keyword:keywords . t) (ess-R-fl-keyword:assign-ops . t) (ess-R-fl-keyword:constants . t) (ess-fl-keyword:fun-calls . t) (ess-fl-keyword:numbers . t) (ess-fl-keyword:operators . t) (ess-fl-keyword:delimiters . t) (ess-fl-keyword:= . t) (ess-R-fl-keyword:F&T . t))))
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" browse-url file) ("pdf" . "evince %s")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; temporary workaround for auto-complete being very slow
(ac-flyspell-workaround)

(provide 'init)
;;; init.el ends here
