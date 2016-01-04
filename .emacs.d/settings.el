;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; cursor doesn't blink
(blink-cursor-mode -1)
(when (display-graphic-p)
  (setq-default cursor-type 'box))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))

;; changing the font
;;(set-default-font "Fantasque Sans Mono")
;;(set-frame-font "Fantasque Sans Mono")
(set-face-attribute 'default nil :height 115)
;; changing tabs in text mode
(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             (setq indent-line-function (quote insert-tab))))

;; yes or no to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'solarized-light t)

(setq x-underline-at-descent-line t)
;; make the fringe stand out from the background
(setq solarized-distinct-fringe-background t)
;; make the modeline high contrast
(setq solarized-high-contrast-mode-line t)
;; Use less bolding
(setq solarized-use-less-bold t)
;; Use more italics
(setq solarized-use-more-italic t)
;; Use less colors for indicators such as git:gutter, flycheck and similar.
(setq solarized-emphasize-indicators nil)

;; Avoid all font-size changes
(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)

;; mode line
;; (setq sml/theme 'light)
;; (setq sml/no-confirm-load-theme t)
;; (sml/setup)
;; (setq sml/shorten-directory t)
;; (setq sml/shorten-modes t)

;; Indicate where a buffer stars and stops
(setq-default indicate-buffer-boundaries 'right)

;; changing where all the backups go
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t)))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

;; More keys you can use for rebinding M-k
(global-set-key (kbd "C-j") 'backward-char)
(global-set-key (kbd "M-j") 'backward-word)
(global-set-key (kbd "C-S-s") 'replace-regexp)
(global-set-key (kbd "M-s") 'replace-string)
(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)
(global-set-key (kbd "C-M-i") 'cleanup-buffer)
(global-set-key (kbd "C-;") 'backward-delete-char-untabify)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Translate the problematic keys to the function key Hyper:
(keyboard-translate ?\C-i ?\H-i)
(keyboard-translate ?\C-m ?\H-m)
;; Rebind then
(global-set-key [?\H-m] 'next-line)
(global-set-key [?\H-i] 'previous-line)

;; get rid of Emacs error noise
;;(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; Move more quickly up and down
(global-set-key (kbd "M-m")
            (lambda ()
              (interactive)
              (ignore-errors (next-line 5))))
(global-set-key (kbd "M-i")
             (lambda ()
              (interactive)
              (ignore-errors (previous-line 5))))

;; line numbers
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun my-split-window-vertical ()
  "Vertical Split window with another buffer."
  (interactive)
  (select-window (split-window-right))
  (switch-to-buffer (other-buffer)))

(defun my-split-window-horizontal ()
  "Vertical Split window with another buffer."
  (interactive)
  (select-window (split-window-below))
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "C-x 2") #'my-split-window-horizontal)
(global-set-key (kbd "C-x 3") #'my-split-window-vertical)

;;Keep open files open across sessions
(desktop-save-mode 1)
(setq desktop-restore-eager 2)
(add-to-list 'desktop-modes-not-to-save 'dired-mode)

;; Change title frame!
(setq frame-title-format '("best editor in existence"))
;; Ban whitespace at end of lines, globally
(add-hook 'write-file-hooks
          '(lambda ()
             (delete-trailing-whitespace)))

;; delete not kill it into kill-ring
;; http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this many times.
This command does not push erased text to kill-ring."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push erased text to kill-ring."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "M-;") 'backward-delete-word)

;; Display the time
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;;(setq display-time-format "%1:%M%p")

;; When you start typing with text selected, replace it with what you're typing
(delete-selection-mode 1)

;; Resizing mini buffers
(setq resize-mini-windows t)
(setq max-mini-window-height 0.33)

;; Opens Emacs window to side instead of below
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; rotate-windows()
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; C-x C-k kills file of current buffer
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

;; move-line-up move-line-down
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<M-down>") 'move-line-down)
(global-set-key (kbd "<M-up>") 'move-line-up)

;; opening new lines quickly
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; refresh contents of buffers if file changes on disk
(global-auto-revert-mode 1)

;; japanese / english font size pairs
(when (window-system)
  (defvar emacs-english-font "Consolas"
    "The font name of English.")

  (defvar emacs-cjk-font "BatangChe"
    "The font name for CJK.")

  (defvar emacs-font-size-pair '(17 . 28)
    "Default font size pair for (english . japanese)")

  (defvar emacs-font-size-pair-list
    '(( 5 .  6) (9 . 10) (10 . 12)(12 . 18)
      (13 . 19) (15 . 21) (17 . 23) (19 . 25)
      (20 . 26) (21 . 27) (24 . 30) (26 . 34)
      (28 . 36) (30 . 38) (34 . 42) (36 . 46))
    "This list is used to store matching (englis . japanese) font-size.")

  (defun font-exist-p (fontname)
    "Test if this font is exist or not."
    (if (or (not fontname) (string= fontname ""))
        nil
      (if (not (x-list-fonts fontname)) nil t)))

  (defun set-font (english japanese size-pair)
    "Setup emacs English and japanese font on x window-system."

    (if (font-exist-p english)
        (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

    (if (font-exist-p japanese)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset
                            (font-spec :family japanese :size (cdr size-pair))))))
  ;; Setup font size based on emacs-font-size-pair
  (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)

  (defun emacs-step-font-size (step)
    "Increase/Decrease emacs's font size."
    (let ((scale-steps emacs-font-size-pair-list))
      (if (< step 0) (setq scale-steps (reverse scale-steps)))
      (setq emacs-font-size-pair
            (or (cadr (member emacs-font-size-pair scale-steps))
                emacs-font-size-pair))
      (when emacs-font-size-pair
        (message "emacs font size set to %.1f" (car emacs-font-size-pair))
        (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

  (defun increase-emacs-font-size ()
    "Decrease emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size 1))

  (defun decrease-emacs-font-size ()
    "Increase emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size -1))

  (global-set-key (kbd "C-=") 'increase-emacs-font-size)
  (global-set-key (kbd "C--") 'decrease-emacs-font-size)
  )
