;;; library.el

(defvar library-org-file
  (concat current-user-directory
	  "Library/library.org")
  "Path to `library.org' file.")

(defvar library-bib-file
  (concat current-user-directory
	  "Library/library.bib")
  "Path to `library.bib' file.")

(defvar library-resources-directory
  (concat current-user-directory
	  "Library/resources/")
  "Path to the `resources' directory.")

(defun library-find-org-file ()
  "Find `library-org-file' in Read-Only mode."
  (interactive)
  (find-file-read-only library-org-file))

(defun library-dired-resources-directory ()
  "Open `library-resources-direcotry' with Dired."
  (interactive)
  (dired library-resources-directory))

(defvar library-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'library-search-show-narrowed)
    map)
  "Keymap used Library Search mode.")

(define-minor-mode library-search-mode
  "Library Search mode is a minor mode Org-Agenda buffer.

Its main purpose is overwrite the binding of
`org-agenda-show-and-scroll-up' in Org-Agenda buffers and use
`library-search-show-narrowed' instead."
  :keymap library-search-mode-map
  :lighter " Lbr")

(defun library-search-show-narrowed (&optional un-narrow)
  "Display the entry at point in `library-org-file' with the
buffer narrowed to the entry only.

If UN-NARROW is given as \\[universal-argument], the buffer is
not narrowed."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (unless un-narrow
      (org-narrow-to-element))
    (select-window win)))

(defun library-search ()
  "Run `org-agenda' in `library-org-file'."
  (interactive)
  (let ((org-agenda-files (list library-org-file))
	(list-matching-lines-default-context-lines 3)
	org-tags-match-list-sublevels
	org-agenda-include-diary
	org-agenda-include-deadlines
	org-agenda-use-time-grid
	org-habit-show-habits)
    (org-agenda)
    (let ((occur-buffer (get-buffer "*Occur*")))
      (if occur-buffer
	  (select-window (get-buffer-window occur-buffer))
	(library-search-mode)))))

(defun library-find-bib-file ()
  "Find `library-bib-file' in Read-Only mode."
  (interactive)
  (find-file-read-only library-bib-file))

(defun library-make-single-bibtex-field (field &optional opt)
  "Outputs the string for on single BibTeX FIELD including equal
sign, braces, comma and line break.  FIELD must be an element
obtained via `bibtex-field-list'.  If OPT is passed, `APT' will
be pre-appended to the field name."
  (let ((field-string "  "))
    (unless (consp field)
      (setq field (list field)))
    ;; Initial string
    (cond
     ((nth 3 field)
      (setq field-string "  ALT"))
     (opt
      (setq field-string "  OPT")))
    ;; Up to equal sign
    (setq field-string
	  (concat field-string
		  (car field)
		  " = "))
    ;; Indentation
    (while (< (length field-string)
	      bibtex-text-indentation)
      (setq field-string
	    (concat field-string " ")))
    ;; Delimiters
    (concat field-string "{},\n")))

(defun library-bibtex-entry (entry-type)
  "Outputs the string for BibTeX entry ENTRY-TYPE.  ENTRY-TYPE
will be passed to `bibtex-field-list' to recover the field names
and each field name will be accordingly passed to
`library-make-single-bibtex-field' to generate each line of the
entry."
  (let ((field-list (bibtex-field-list entry-type))
	(entry (concat "@" entry-type "{<entry id>,\n")))
    (concat
     (substring
      (apply
       'concat
       (append (list entry)
	       (mapcar 'library-make-single-bibtex-field
		       (car field-list))
	       (mapcar #'(lambda (field)
			   (library-make-single-bibtex-field field 'optional))
		       (cdr field-list))))
      0 -2)
     "\n}\n")))

(defun library-capture-template (entry-type &optional category)
  "Generate a capture template based on ENTRY-TYPE.
CATEGORY is the category to be placed in the `:CATEGORY:' field
in the properties drawer of the entry.  See
`library-bibtex-entry' for information on ENTRY-TYPE."
  (concat
   "* Title%? :keywords:
:PROPERTIES:
:ID: <entry id>"
   (when category
     (format "%s%s" "\n:CATEGORY: " category))
   "\n:END:
- Added: %t
- Year: 
- Author: 

** Resources
- [[https://]]
- [[file:]]


** Citation
#+begin_src bibtex\n"
   (library-bibtex-entry entry-type)
   "#+end_src

** TODO Summary
TBD."))

(defun library-generate-capture-template (key entry-type description headline)
  "Generate a template for a library entry to be used in `org-capture'.
KEY is the key to select the type of entry in the selection
buffer; ENTRY-KEY is the corresponding BibTeX key for the entry;
DESCRIPTION is the description to that key in that frame;
HEADLINE is the heading under which it will appear in the
library's org file.  The first word in DESCRIPTION will be passed
as the CATEGORY argument to `library-capture-template'."
  `(,key
    ,description
    entry
    (file+headline
     ,(concat current-user-directory "library/library.org")
     ,headline)
    ,(library-capture-template
      entry-type
      (car (split-string description)))
    :jump-to-captured t
    ))

;; Set custom capture templates to org-capture
(with-eval-after-load 'org-capture
  (bibtex-set-dialect)
  (mapc (lambda (elem) (add-to-list 'org-capture-templates elem))
	;; List in inverse alphabetical order
	`(,(library-generate-capture-template
	    "lo" "booklet"
	    "Other" "Other references")
	  ,(library-generate-capture-template
	    "lt" "phdthesis"
	    "Thesis" "Books and theses")
	  ,(library-generate-capture-template
	    "lj" "article"
	    "Journal paper" "Journal papers")
	  ,(library-generate-capture-template
	    "lc" "inproceedings"
	    "Conference paper" "Conference papers")
	  ,(library-generate-capture-template
	    "lb" "book"
	    "Book" "Books and theses")
	  ("l" "Library")
	  )))

(defun library-bibtex-clean-entry ()
  "Find a `bibtex' source block and run `bibtex-clean-entry'."
  (interactive)
  (unless org-note-abort
    (let* ((file-name
	    (buffer-file-name
	     (plist-get org-capture-plist :buffer))))
      (when (string=
	     (upcase file-name)
	     (upcase library-org-file))
	(save-excursion
	  (goto-char (point-min))
	  (when (search-forward "begin_src bibtex" nil t)
	    (next-line)
	    (ignore-error nil (bibtex-clean-entry))))))))

(add-hook 'org-capture-prepare-finalize-hook
	  'library-bibtex-clean-entry)

(defun library-tangle-file ()
  "Update `library-bib-file' by running `org-babel-tangle-file' on
`library-org-file' and overwriting the existing file."
  (interactive)
  (require 'ob-tangle)
  (org-babel-tangle-file library-org-file library-bib-file))

(defalias 'library-add-entry 'org-capture
  "Set an alias to avoid confusion with possible future usages of
org-capture.")

;; Set up global key bindings for the library
(global-set-key (kbd "C-c l o") 'library-find-org-file)
(global-set-key (kbd "C-c l s") 'library-search)
(global-set-key (kbd "C-c l b") 'library-find-bib-file)
(global-set-key (kbd "C-c l a") 'library-add-entry)
(global-set-key (kbd "C-c l t") 'library-tangle-file)
(global-set-key (kbd "C-c l r") 'library-dired-resources-directory)
