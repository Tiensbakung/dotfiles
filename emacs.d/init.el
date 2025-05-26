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
(transient-mark-mode t)	                  ;; Highlight selected region
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
(setq ring-bell-function (lambda ()))     ;; No bell

(mouse-avoidance-mode 'animate)	          ;; Auto mouse avoidance of curosr
(setq mouse-yank-at-point t)              ;; Paste at point not cursor
(setq select-enable-clipboard nil)

(column-number-mode t)
(global-hl-line-mode t)	                  ;; Highlight current line
(show-paren-mode t)                       ;; Highlight matching parenthesis
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

(load-theme 'modus-operandi-deuteranopia)
(set-frame-parameter nil 'alpha-background 75)
(set-frame-font "Fira Code-13" nil t)

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
(global-set-key [f5] 'recompile)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c e n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c e p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'text-mode-hook 'flyspell-mode)
(setq c-default-style "linux")
(setq c-basic-offset 4)
(add-hook 'java-mode-hook (lambda () (setq c-basic-offset 4)))
(require 'ansi-color)
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))
(setq compilation-scroll-output 'first-error)
(setf epg-pinentry-mode 'loopback)

(require 'ispell)
(require 'ibuffer)
(setq ibuffer-expert t)
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode t)
(setq default-directory "~/Workspace/")

(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 0)
(setq web-mode-script-padding 0)
(setq web-mode-block-padding 0)
(setq web-mode-enable-auto-expanding t)
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-interpolation t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-engines-alist
      '(("vue" . "\\.vue\\'")
        ("svelte" . "\\.svelte\\'")))
(setq js-indent-level 2)

(require 'iedit)
;; (global-set-key (kbd "C-\"") 'iedit-mode)
(setq dash-docs-browser-func 'eww)

(require 'yasnippet)
(yas-global-mode 1)

(require 'avy)
(avy-setup-default)
(global-set-key (kbd "C-:") 'avy-goto-char-timer)
(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "C-c C-j") 'avy-resume)
(global-set-key (kbd "M-o") 'ace-window)

(require 'project)
(use-package project
  :init
  (setq project-vc-extra-root-markers '("requirements.txt")))
(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )
(use-package savehist :init (savehist-mode))
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-\"" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g s" . consult-lsp-symbols)
         ;; M-s bindings in `search-map'
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(recentf-mode)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

;; tree-sitter
(setq major-mode-remap-alist
	  '((python-mode . python-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (js-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (java-mode . java-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (go-mode . go-ts-mode)))

(defun my-indent-style()
  "Override the built-in BSD indentation style with some additional rules"
  `(;; Here are your custom rules
    ((node-is ")") parent-bol 0)
    ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "argument_list") prev-sibling 0)
    ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
    ((parent-is "parameter_list") prev-sibling 0)
    ;; Append here the indent style you want as base
    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(use-package c-ts-mode
 :if (treesit-language-available-p 'c)
 :custom
 (c-ts-mode-indent-offset 4)
 (c-ts-mode-indent-style #'my-indent-style))

;; eglot
(require 'eglot)
(add-to-list 'exec-path "/home/tiens/.node_modules/bin")
;; web-mode setup
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.geojson\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.prisma\\'" . prisma-ts-mode))
(defun my-web-mode-setup ()
  (setq tab-width 2))
(defun my-python-setup ()
  (setq tab-width 4))
(add-hook 'web-mode-hook 'my-web-mode-setup)
(add-hook 'js-ts-mode-hook 'my-web-mode-setup)
(add-hook 'css-ts-mode-hook 'my-web-mode-setup)
(add-hook 'python-ts-mode-hook 'my-python-setup)
(add-hook 'tsx-ts-mode-hook 'my-web-mode-setup)
(add-hook 'typescript-ts-mode-hook 'my-web-mode-setup)

(defun vue-eglot-init-options ()
  (let ((serverPath
         (expand-file-name
          "lib"
          (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))))
    `(:typescript
      (:tsdk "/home/tiens/.node_modules/lib/node_modules/typescript/lib"
             :languageFeatures (:completion
                                (:defaultTagNameCase
                                 "both"
                                 :defaultAttrNameCase "kebabCase"
                                 :getDocumentNameCasesRequest nil
                                 :getDocumentSelectionRequest nil)
                                :diagnostics
                                (:getDocumentVersionRequest nil))
             :documentFeatures (:documentFormatting
                                (:defaultPrintWidth
                                 100 :getDocumentPrintWidthRequest nil)
                                :documentSymbol t
                                :documentColor t)))))
;; Volar
(add-to-list 'eglot-server-programs
             `(vue-mode . ("vue-language-server" "--stdio"
                           :initializationOptions
                           ,(vue-eglot-init-options))))
;; (add-to-list 'eglot-server-programs
;;              `(typescript-ts-mode . ("vue-language-server" "--stdio"
;;                                      :initializationOptions
;;                                      ,(vue-eglot-init-options))))
(global-set-key (kbd "C-c l f") 'eglot-format)
(global-set-key (kbd "C-c l a") 'eglot-code-actions)
(global-set-key (kbd "C-c l r") 'eglot-rename)

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

(require 'diminish)
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)
(diminish 'flyspell-mode)
(diminish 'yas-minor-mode)

(add-hook 'w3m-display-hook
          (lambda (url)
            (let ((buffer-read-only nil))
              (delete-trailing-whitespace))))

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
 '(package-selected-packages
   '(avy dash-docs diminish eglot eldoc embark-consult iedit magit marginalia
         orderless org-tree-slide ox-reveal prisma-ts-mode vertico w3m web-mode
         yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
