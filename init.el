;; -*- lexical-binding: t -*-
;;; package --- repo
;;; Commentary:
;;; Personal Emacs init.el
;;; Code:
(setq package-archives
      '(
        ;; ("gnu" . "http://elpa.gnu.org/packages/")
        ("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
        ;; ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ))

;; set emacs encoding
(setq current-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Show more info on the title
(setq-default frame-title-format (list "[%b]: %+ %f"))
;; (list invocation-name "@" system-name ":%b %+%+ %f"))

(setq inhibit-startup-message t)          ;; Don't show GNU splash screen
(setq-default make-backup-files nil)      ;; No backup file
(setq ad-redefinition-action 'accept)
(fset 'yes-or-no-p 'y-or-n-p)
(transient-mark-mode t)                   ;; Highlight selected region
(electric-pair-mode t)
(electric-indent-mode)
(delete-selection-mode 1)
(global-font-lock-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)                        ;; No toolbar
(scroll-bar-mode -1)                      ;; No scroll bar
(blink-cursor-mode -1)
(global-auto-revert-mode 1)               ;; Refresh a opened file

(setq invisible-bell t)
(setq ring-bell-function (lambda ()))          ;; No bell

(mouse-avoidance-mode 'animate)           ;; Auto mouse avoidance of curosr
(setq mouse-yank-at-point t)              ;; Paste at point not cursor
(setq select-enable-clipboard nil)

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
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq image-use-external-converter t)

;; Appearance customization
;; (set-frame-font "Monospace-11" nil t)
(setq default-frame-alist '((top . 25) (left . 0) (height . 34) (width . 84)))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-.") 'complete-symbol)
;; (global-set-key (kbd "C-w") 'backward-kill-word)
;; (global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key [f5] 'recompile)
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(global-set-key (kbd "C-c e n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c e p") 'flymake-goto-prev-error)
;; auto mode association
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Spell checking
;; (setq ispell-program-name "aspell")
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-mode)


(setq c-default-style "linux")
(setq c-basic-offset 4)
;; (add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4)))
(setq default-directory "~/Workspace/")

(require 'ansi-color)
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))
(setq compilation-scroll-output 'first-error)

(setf epa-pinentry-mode 'loopback)

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; ibuffer
(require 'ibuffer)
(setq ibuffer-expert t)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode t)

(require 'ispell)
;; (which-function-mode t)
;; (global-flycheck-mode t)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 0)
(setq web-mode-script-padding 0)
(setq web-mode-block-padding 0)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-engines-alist '(("vue" . "\\.vue\\'")))
(setq js-indent-level 2)

(require 'iedit)
(global-set-key (kbd "C-\"") 'iedit-mode)
(setq dash-docs-browser-func 'eww)

;; (require 'dart-mode)
;; (require 'flutter)
;; (setq flutter-sdk-path "/home/tiens/.flutter")
;; (add-hook 'dart-mode-hook
;;           (lambda () (local-set-key (kbd "C-c C-c") 'flutter-run-or-hot-reload)))

;; lsp mode
(require 'lsp-mode)
(require 'lsp-java)
(require 'lsp-ui)
(require 'lsp-jedi)
(require 'lsp-java-boot)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'web-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'go-mode-hook #'lsp)
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace)
  (add-to-list 'lsp-disabled-clients 'pyls))
(setq lsp-vetur-emmet "inMarkupAndStylesheetFilesOnly")
(setq lsp-vetur-format-default-formatter-html "prettier")
;; (setq lsp-vetur-experimental-template-interpolation-service t)
(setq lsp-modeline-code-actions-enable nil
      lsp-ui-sideline-show-code-actions nil
      lsp-diagnostics-provider :flymake
      lsp-headerline-breadcrumb-enable 1
      lsp-ui-sideline-update-mode 'point
      lsp-ui-doc-show-with-cursor nil
      lsp-ui-doc-enable t
      lsp-lens-enable nil
      gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      )
(global-set-key (kbd "C-c l i") 'lsp-iedit-highlights)
(global-set-key (kbd "C-c l a") 'lsp-execute-code-action)
(global-set-key (kbd "C-c l r") 'lsp-rename)
(global-set-key (kbd "C-c l h") 'lsp-describe-thing-at-point)
(global-set-key (kbd "C-c l f") 'lsp-format-buffer)

;; modus theme
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-region '(bg-only no-extend))

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(require 'avy)
(avy-setup-default)
(global-set-key (kbd "C-'") 'avy-goto-char-timer)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "C-c C-j") 'avy-resume)
(global-set-key (kbd "M-o") 'ace-window)

(require 'magit)

(require 'project)
(require 'vertico)
(require 'consult)
(require 'embark)
(require 'marginalia)
(vertico-mode)
(marginalia-mode)
(recentf-mode)
(savehist-mode)
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq enable-recursive-minibuffers t)
(setq completion-styles '(substring orderless basic partial-completion flex)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(global-set-key (kbd "C-c h") 'consult-history)
(global-set-key (kbd "C-c r") #'vertico-repeat)
(global-set-key (kbd "C-x M-:") 'consult-complex-command)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)
(global-set-key (kbd "C-x r b") 'consult-bookmark)
(global-set-key (kbd "C-x p b") 'consult-project-buffer)

(global-set-key (kbd "M-\"") 'consult-register-load)
(global-set-key (kbd "M-'") 'consult-register-store)
(global-set-key (kbd "C-M-#") 'consult-register)
(global-set-key (kbd "M-y") 'consult-yank-pop)
(global-set-key (kbd "C-h a") 'consult-apropos)

(global-set-key (kbd "M-g e") 'consult-compile-error)
(global-set-key (kbd "M-g f") 'consult-lsp-diagnostics)
(global-set-key (kbd "M-g o") 'consult-outline)
(global-set-key (kbd "M-g m") 'consult-mark)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "C-c i") 'consult-imenu)
(global-set-key (kbd "M-g I") 'consult-imenu-multi)

(global-set-key (kbd "M-s r") 'consult-ripgrep)
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s L") 'consult-line-multi)
(global-set-key (kbd "M-s k") 'consult-keep-lines)
(global-set-key (kbd "M-s u") 'consult-focus-lines)
(global-set-key (kbd "M-g s") 'consult-lsp-symbols)
(global-set-key (kbd "C-;") 'embark-act)
;; (advice-add #'register-preview :override #'consult-register-window)
;; (advice-add #'completing-read-multiple
;;             :override #'consult-completing-read-multiple)
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark (require 'embark-consult)))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(require 'nodejs-repl)
(add-hook 'js-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

;; org
(require 'org)
(require 'ox-latex)
(require 'ob-dot)
(eval-after-load "org" '(require 'ox-md nil t))
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")
        ("linenos" "")
        ("breaklines" "")))
(setq org-latex-pdf-process (list "latexmk -shell-escape -pdf %f"))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-default-notes-file "~/Workspace/org/notes.org")
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (dot . t)
   (haskell . t)
   (java . t)
   (perl . t)
   (python . t)
   (scheme . t)
   (R . t)
   (js . t)
   (gnuplot . t)
   (latex . t)
   (sql . t)
   (sqlite . t)
   (lua . t)
   (shell . t)
   (ruby . t)))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
(add-to-list 'org-latex-classes
             '("myreport"
               "\\documentclass{report}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; (require 'ess-site)

(require 'diminish)
(diminish 'projectile-mode " P")
(diminish 'lsp-mode " L")
;; (diminish 'company-mode)
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)
(diminish 'flyspell-mode)
(diminish 'yas-minor-mode)

;; remove trailing white spaces in w3m
(add-hook 'w3m-display-hook
          (lambda (url)
            (let ((buffer-read-only nil))
              (delete-trailing-whitespace))))

(autoload 'po-mode "po-mode"
  "Major mode for translators to edit PO files" t)
(setq auto-mode-alist (cons '("\\.po\\'\\|\\.po\\." . po-mode)
                            auto-mode-alist))
(autoload 'po-find-file-coding-system "po-compat")
(modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
                            'po-find-file-coding-system)

(require 'ox-reveal)
(setq org-reveal-root "file:///home/tiens/.node_modules/lib/node_modules/reveal.js/")

(require 'org-tree-slide)
(global-set-key (kbd "<f8>") 'org-tree-slide-mode)
(global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
(define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
(define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree)
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dichromacy))
 '(custom-safe-themes
   '("62bbec37a4404710ff1dc9767e4276cdbde77febde3b3f8f9087d0f4bd1ff3c3" "59820d0ca7ba29984ab4d255f9fcfaa28041ad6ae4b6ae2ffb2ccfbfaec210e4" "8b8fd1c936a20b5ca6afe22e081798ffb5e7498021515accadc20aab3517d402" default))
 '(doc-view-continuous t)
 '(ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)))
 '(global-prettify-symbols-mode t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(org-agenda-files '("~/Workspace/org/notes.org"))
 '(org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" browse-url file)
     ("pdf" . "evince %s")))
 '(package-selected-packages
   '(yaml-mode timu-macos-theme magit nova-theme termbright-theme tommyh-theme twilight-bright-theme zeno-theme orderless vertico consult-dash dash-docs htmlize iedit org-modern expand-region nodejs-repl eldoc yasnippet-snippets embark embark-consult consult consult-lsp marginalia lsp-jedi web-mode org-tree-slide ox-reveal lsp-ui go-mode w3m lsp-java lsp-python lsp-intellij lsp-mode restclient markdown-mode graphviz-dot-mode diminish))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
