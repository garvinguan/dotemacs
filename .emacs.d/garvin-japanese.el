;; Various utilities to get definitions from a Japanese dictionary file
;; Josh Moller-Mara's code adapted for Japanese
;; - Garvin Guan

(defvar garvin/japanese-dictionary-path
  (concat (file-name-directory load-file-name) "edict2.u8")
  "Where we store the Japanese dictionary edict2.u8")

(defun garvin/japanese-prompt ()
  "Prompt for a character, return it"
  (setq garvin/japanese-word (read-from-minibuffer "Word/Phrase: ")))


(defun garvin/japanese-dict-find (phrase)
  "Find a japanese word or phrase in the dictionary"
  (with-temp-buffer
    (insert-file-contents garvin/japanese-dictionary-path)
    (let (definitions)
      (while (re-search-forward (concat "^[^][]*\\b" phrase "\\b.*?$") nil t)
        (push (buffer-substring (match-beginning 0)
                                (match-end 0))
              definitions))
      (setq garvin/japanese-word-dict
            (if (equal (length definitions) 1)
                (car definitions)
              (helm-comp-read "Pick a definition: "
                              definitions
                              :nomark t))))))

(defun garvin/japanese-get-pronunciation (dictentry)
  "Get a pronunciation from a dictionary entry."
  (save-match-data
    (and (string-match "\\[\\(.*?\\)\\]" dictentry)
         (format "[%s]" (match-string 1 dictentry)))))

(defun garvin/japanese-get-definition (dictentry)
  "Get a definition from a dictionary entry."
  (let ((index 0))
    (setq entry (and (string-match "/\\(.*?\\)$" dictentry)
                     (match-string 1 dictentry)))
    (mapconcat (lambda (str) (format "%s. %s" (incf index) str))
               (split-string entry "/" t)
               "\n")))

(defun garvin/japanese-get-word (dictentry)
  "Return either the character, or a list of traditional and simplified."
  (let* ((words (and (string-match "^\\(.+?\\) \\[" dictentry)
                     (match-string 1 dictentry)))
         (garvin/japanese-words (split-string words ";" t)))
    (message "s" garvin/japanese-words)
    (if (> (length garvin/japanese-words) 1)
        (mapconcat (lambda (str) (format "[%s] " str))
                   garvin/japanese-words
                   ""))))

(defun garvin/japanese-def-at-point (&optional arg)
  "Get the definition of a character at the point and display in
the minibuffer. With an argument, insert the definition into the
buffer."
  (interactive "P")
  (let ((phrase (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (string (char-after))))
        definitions)
    (with-temp-buffer
      (insert-file-contents garvin/japanese-dictionary-path)
      (while (re-search-forward (concat "^[^][]*\\b" phrase "\\b.*?$") nil t)
        (push (buffer-substring (match-beginning 0)
                                (match-end 0))
              definitions)))
    (let ((defs (mapconcat 'identity definitions "\n")))
      (if arg
          (insert defs)
        (message defs)))))

(global-set-key (kbd "<f9> C") 'garvin/japanese-def-at-point)

(defvar garvin/japanese-decomposition-path
  (concat (file-name-directory load-file-name) "cjk-decomp-0.4.0.txt")
  "Where we store the Japanese character decomposition data")

(defun garvin/japanese-decomposition-find (phrase)
  "Find a japanese word or phrase in the dictionary"
  (defun str-decomp (strnum)
    (if (and strnum (= (string-to-number strnum) 0))
        strnum
      (garvin/japanese-decomposition-find strnum)))
  (with-temp-buffer
    (insert-file-contents garvin/japanese-decomposition-path)
    (let (definitions)
      (when (re-search-forward (concat "^" phrase ":.*?$") nil t)
        (let ((decomp (buffer-substring (match-beginning 0)
                                        (match-end 0))))
          (save-match-data
            (string-match "^\\(.*?\\):\\(.*?\\)(\\(.*?\\))$" decomp)
            (let* ((decomptype (match-string 2 decomp))
                   (constituents (split-string (match-string 3 decomp) "[,()]")))
              (cons decomptype (mapcar 'str-decomp constituents)))))))))

(defun garvin/japanese-decomposition-at-point ()
  "Get the decomposition of a character at the point and insert it."
  (interactive)
  (defun listtostr (x)
    (if (listp x)
        (mapconcat 'listtostr x "")
      x))
  (let ((phrase (string (char-after))))
    (insert (listtostr (garvin/japanese-decomposition-find phrase)))))

(global-set-key (kbd "<f9> E") 'garvin/japanese-decomposition-at-point)

(provide 'garvin-japanese)
