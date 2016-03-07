(load-file "~/.emacs.d/settings.el")
(load-file "~/.emacs.d/private.el")

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar my/install-packages
  '(
    ;; package management
    use-package

    ;; themeing
    color-identifiers-mode
    color-theme-sanityinc-tomorrow apropospriate-theme
    material-theme smart-mode-line beacon aurora-theme
    solarized-theme

    ;; misc
    diminish

    ;; for auto-complete
    fuzzy company auto-complete

    ;; editing utilities
    evil expand-region yasnippet multiple-cursors key-chord ace-jump-mode

    ;; highlighting
    idle-highlight-mode

    ;; org-mode
    org org-bullets

    ;; buffer utils
    popwin dired+ ssh-agency

    ;; flycheck
    flycheck flycheck-tip flycheck-pos-tip

    ;; python
    virtualenvwrapper

    ;; go
    go-mode company-go

    ;; java
    javap-mode emacs-eclim java-imports

    ;; javascript
    tern json-mode js2-mode ac-js2 impatient-mode web-mode

    ;; helm
    helm helm-descbinds helm-ag helm-projectile helm-swoop
    helm-gtags helm-ls-git helm-flycheck helm-flyspell helm-flx
    helm-c-yasnippet

    ;; git
    magit

    ;; eww
    eww-lnum
    ))

(defvar packages-refreshed? nil)

(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (unless packages-refreshed?
      (package-refresh-contents)
      (setq packages-refreshed? t))
    (unwind-protect
	(condition-case ex
	    (package-install pack)
	  ('error (message "Failed to install package [%s], caught exception: [%s]"
			   pack ex)))
      (message "Installed %s" pack))))

;; Load use-package, used for loading packages everywhere else
(require 'use-package)
(use-package erc-log)
(use-package diminish)
(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (add-hook #'org-mode-hook #'org-indent-mode)
  (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" " DONE(d)")
			    (sequence "⚑ WAITING(w)" "|")
			    (sequence "|" "✘ CANCELED(c)")))
  (bind-key "C-j" 'backward-char org-mode-map)
  (bind-key "M-e" 'toggle-truncate-lines org-mode-map)
  (bind-key "C-;" 'backward-delete-char org-mode-map)
  (bind-key "M-;" 'backward-delete-word org-mode-map)
  (bind-key "M-j" 'backward-word org-mode-map)
  (bind-key "C-j" 'backward-char org-mode-map)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-log-done t)
  (setq org-capture-templates
	'(("i" "importanttodo" entry (file+headline "~/org/notes.org" "Important")
	   "* TODO  %?\n  %U\n")
	  ("w" "websitetodo" entry (file+headline "~/org/website.org" "What to work on next")
	   "* TODO  %?")
	  ("n" "interesting things to find out" entry (file+headline "~/org/notes.org" "What to find out next")
	   "* TODO  %?")
	  ("e" "extension" entry (file+headline "~/org/extensiontodos.org" "What to work on next")
	   "* TODO  %?")
	  ("q" "questions" entry (file+headline "~/org/notes.org" "Questions to ask")
	   "* TODO  %?\n  %U\n  %a")
	  ("r" "reminder" entry (file+headline "~/org/notes.org" "Reminder")
	   "* TODO  %?\n  %U\n  %a")
	  ("o" "other" plain (file+headline "~/org/notes.org" "Other")
	   "%?\n")
	  ("g" "go notes" entry (file+headline "~/org/notes.org" "Go notes")
	   "*  %?\n  %U\n  %a")
	  ("j" "journal" entry (file+datetree "~/org/journal.org")
	   "*  %?\n %U\n  %a")))
  (setq org-agenda-files (list "~/org/website.org"
			       "~/org/other.org"
			       "~/org/extensiontodos.org"
			       ;; "~/org/recommendations.org"
			       "~/org/notes.org"
			       )))
(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
	'("*" "✿" "✤" "☀" "▾" "▸"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package company
  :init (add-hook #'prog-mode-hook #'company-mode)
  :diminish company-mode
  :config
  (setq company-tooltip-limit 20                      ; bigger popup window
	company-idle-delay .2                         ; decrease delay before autocompletion popup shows
	company-echo-delay 0                          ; remove annoying blinking
	company-begin-commands '(self-insert-command) ; start autocompletion only after typing
	;; min prefix of 3 chars
	company-selection-wrap-around t
	company-show-numbers t
	company-dabbrev-downcase nil
	company-transformers '(company-sort-by-occurrence))
  (bind-keys :map company-active-map
	     ("C-n" . company-select-next)
	     ("C-p" . company-select-previous)
	     ("C-d" . company-show-doc-buffer)
	     ("<tab>" . company-complete))
  )

(use-package go-mode
  :config
  (use-package go-mode-autoloads)
  (use-package company-go)
  (add-hook 'go-mode-hook (lambda ()
			    (set (make-local-variable 'company-backends) '(company-go))
			    (company-mode))))

(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (js2-imenu-extras-setup)
  :config
  (use-package ac-js2)
  (bind-key "C-j" 'js2-jump-to-definition js2-mode-map)
  )

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  :config
  (bind-keys :map web-mode-map
            ("C-j" . backward-char)
            ("M-j" . backward-word)
            ("C-;" . backward-delete-char)
            ("M-;" . backward-delete-word))
  (use-package impatient-mode
    :config
    (defun impatient-web-mode-hook ()
      "Starts the `simple-httpd' server if it is not already running, and turns
on `impatient-mode' for the current buffer."
      (unless (get-process "httpd")
	(message "starting httpd server...")
	(httpd-start))
      (impatient-mode))
    (add-hook #'web-mode-hook #'impatient-web-mode-hook)))


(use-package key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay .040
	key-chord-one-key-delay .135)
  (key-chord-define-global "jl" 'find-file)
  (key-chord-define-global ";s" 'save-buffer)
  (key-chord-define-global "ef" 'eval-buffer)
  (key-chord-define-global "kl" 'switch-to-buffer)
  ;; (key-chord-define-global "kl" 'switch-to-buffer-other-window)
  (key-chord-define-global ";w" 'other-window))

(use-package expand-region
  :bind (("C-," . er/expand-region)
	 ("C-." . er/contract-region)))

;; multiple cursors
(use-package multiple-cursors
  :bind (("C-c C-l" . mc/edit-lines)
	 ("C-c H-m" . mc/mark-next-like-this)
	 ("C-c H-i" . mc/mark-previous-like-this)
	 ("C-c C-a" . mc/mark-all-like-this)
	 ("C-c C-r" . mc/mark-all-in-region)))

(use-package ace-jump-mode
  :bind ("C-o" . ace-jump-mode))

(use-package helm-flx
  :init (helm-flx-mode +1))

(use-package helm
  :diminish helm-mode
  :config
  (helm-mode 1)
  (bind-keys :map helm-map
             ("C-j" . helm-next-line)
             ("C-f" . helm-execute-persistent-action)
             ("C-k" . helm-previous-line)))

(use-package projectile
  :defer 5
  :commands projectile-global-mode
  :diminish projectile-mode
  :config
  (bind-key "C-c p b" #'projectile-switch-to-buffer #'projectile-command-map)
  (bind-key "C-c p K" #'projectile-kill-buffers #'projectile-command-map)

  ;; global ignores
  (add-to-list 'projectile-globally-ignored-files ".tern-port")
  (add-to-list 'projectile-globally-ignored-files "GTAGS")
  (add-to-list 'projectile-globally-ignored-files "GPATH")
  (add-to-list 'projectile-globally-ignored-files "GRTAGS")
  (add-to-list 'projectile-globally-ignored-files "GSYMS")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  ;; always ignore .class files
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (use-package helm-projectile
    :init
    (use-package grep) ;; required for helm-ag to work properly
    (setq projectile-completion-system 'helm)
    ;; no fuzziness for projectile-helm
    (setq helm-projectile-fuzzy-match nil)
    (helm-projectile-on))
  (projectile-global-mode))

(use-package helm-swoop
  :bind (("M-o" . helm-swoop)
	 ("M-O" . helm-swoop-back-to-last-point)
	 ("C-c M-o" . helm-multi-swoop))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; Save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t
	;; If this value is t, split window inside the current window
	helm-swoop-split-with-multiple-windows nil
	;; Split direcion. 'split-window-vertically or 'split-window-horizontally
	helm-swoop-split-direction 'split-window-horizontally
	;; If nil, you can slightly boost invoke speed in exchange for text color
	helm-swoop-speed-or-color nil))

;; idle-highlight-mode
(use-package idle-highlight-mode
  :config
  (defun my-coding-hook ()
    (make-local-variable 'column-number-mode)
    (column-number-mode t)
    (if window-system (hl-line-mode t))
    (idle-highlight-mode t))
  (add-hook 'java-mode-hook 'my-coding-hook)
  ;; (add-hook 'js2-mode-hook 'my-coding-hook)
  (add-hook 'c++-mode-hook 'my-coding-hook))

(use-package erc
  :init
  (setq erc-nick "workisfun"
	erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-autojoin-channels-alist
	'(("freenode.net" "#emacs-beginners" "#go-nuts"))))

(use-package ediff
  :config
  (progn
    (setq
     ;; Always split nicely for wide screens
     ediff-split-window-function 'split-window-horizontally)))

(use-package flycheck
  :defer 5
  :bind (("M-g M-n" . flycheck-next-error)
	 ("M-g M-p" . flycheck-previous-error)
	 ("M-g M-=" . flycheck-list-errors))
  :init (global-flycheck-mode)
  :diminish flycheck-mode
  :config
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc json-jsonlint json-python-json))
    (use-package flycheck-pos-tip
      :init (flycheck-pos-tip-mode))
    (use-package helm-flycheck
      :init (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))))

(use-package popwin
  :commands popwin-mode
  ;;  :init (popwin-mode 1)
  :config
  (defvar popwin:special-display-config-backup popwin:special-display-config)
  ;;    (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:special-display-config nil)
  ;; basic
  (push '("*Help*" :noselect t :stick t) popwin:special-display-config)
  (push '("*Shell Command Output*" :noselect t :stick t) popwin:special-display-config)

  ;; compilation
  (push '(compilation-mode :stick t :width 0.4) popwin:special-display-config)
  (push '("*Compile-Log*" :stick t :noselect t) popwin:special-display-config)

  ;; magit
  (push '("*magit-process*" :stick t) popwin:special-display-config)

  ;; man
  (push '(Man-mode :stick t :height 20) popwin:special-display-config)

  ;; Elisp
  (push '("*ielm*" :stick t) popwin:special-display-config)
  (push '("*eshell pop*" :stick t) popwin:special-display-config)

  ;; python
  (push '("*Python*"   :stick t) popwin:special-display-config)
  (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
  (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

  (push '("*Occur*" :stick t) popwin:special-display-config)

  ;; org-mode
  (push '("*Org tags*" :stick t :height 30) popwin:special-display-config)

  ;; Completions
  (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)
  )

(use-package beacon
  :defer t
  :diminish beacon-mode
  :init (beacon-mode 1)
  :config
  (setq beacon-blink-duration 0.2)
  (setq beacon-blink-delay 0.1))

(use-package evil
  :config
  (evil-mode 1)
  (popwin-mode) ;; need to figure out how to turn this off
  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-word-mode)
  (define-key evil-motion-state-map (kbd "C-d") 'helm-projectile-find-file)
  (key-chord-define evil-normal-state-map "jk" 'evil-force-normal-state)
  (key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
  ;;  (setq-default evil-cross-lines t)
  ;; different jumps for different visual modes
  (defadvice evil-visual-line (before spc-for-line-jump activate)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))

  (defadvice evil-visual-char (before spc-for-char-jump activate)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

  (defadvice evil-visual-block (before spc-for-char-jump activate)
    (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

  (defun normal-state-after-save ()
    "Return to normal mode after saving"
    (interactive)
    (save-buffer)
    (evil-normal-state))
  (defun org-evil-open-below (count)
    "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (evil-insert-newline-below)
    (setq evil-insert-count count
	  evil-insert-lines t
	  evil-insert-vcount nil)
    (evil-insert-state 1)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces))
  (defun org-evil-open-above (count)
    "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (evil-insert-newline-above)
    (setq evil-insert-count count
	  evil-insert-lines t
	  evil-insert-vcount nil)
    (evil-insert-state 1)
    (add-hook 'post-command-hook #'evil-maybe-remove-spaces))
  (evil-define-key 'normal org-mode-map (kbd "o") 'org-evil-open-below)
  (evil-define-key 'normal org-mode-map (kbd "O") 'org-evil-open-above)
  (key-chord-define evil-insert-state-map ";s" 'normal-state-after-save))

(use-package magit
  :bind (("M-g M-g" . magit-status)
	 ("C-x g" . magit-status))
  :init (add-hook 'magit-mode-hook 'hl-line-mode)
  :config
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  (setenv "GIT_PAGER" "")
  (if (file-exists-p  "/usr/local/bin/emacsclient")
      (setq magit-emacsclient-executable "/usr/local/bin/emacsclient")
    (setq magit-emacsclient-executable (executable-find "emacsclient")))
  (defun my/magit-browse ()
    "Browse to the project's github URL, if available"
    (interactive)
    (let ((url (with-temp-buffer
		 (unless (zerop (call-process-shell-command
				 "git remote -v" nil t))
		   (error "Failed: 'git remote -v'"))
		 (goto-char (point-min))
		 (when (re-search-forward
			"github\\.com[:/]\\(.+?\\)\\.git" nil t)
		   (format "https://github.com/%s" (match-string 1))))))
      (unless url
	(error "Can't find repository URL"))
      (browse-url url)))

  (define-key magit-mode-map (kbd "C-c C-b") #'my/magit-browse))

(load-theme 'solarized-light t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ff0624aca83ba718da04df8e441f03a5033504904a65407bb364e6e6fde96f64" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
