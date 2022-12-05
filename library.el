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

(defun library-make-single-bibtex-field (field &optional opt value)
  "Outputs the string for on single BibTeX FIELD including equal
sign, braces, comma and line break.  FIELD must be an element
obtained via `bibtex-field-list'.

If OPT is passed, \"OPT\" will be pre-appended to the field name.
If VALUE is passed, it will placed as the value of this field."
  (let (field-string)
    ;; Initial string
    (cond
     ((nth 3 field)
      (setq field-string "  ALT"))
     (opt
      (setq field-string "  OPT"))
     (t
      (setq field-string "  ")))
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
    (concat
     field-string
     (if value
	 (format "{%s},\n" value)
       "{},\n"))))

(defun library-bibtex-entry (entry-type &optional values-alist)
  "Outputs the string for BibTeX entry ENTRY-TYPE.  ENTRY-TYPE
will be passed to `bibtex-field-list' to recover the field names
and each field name will be accordingly passed to
`library-make-single-bibtex-field' to generate each line of the
entry.

Elements of VALUES-ALIST should be of the form (FIELD . VALUE),
where FIELD is one of the fields of ENTRY-TYPE and VALUE will be
properly added to the resulting BibTeX entry.  If a FIELD
\"entry-id\" is present, it's value will be the entry's id.
"
  (let ((field-list (bibtex-field-list entry-type))
	(entry
	 (concat
	  "@" entry-type
	  (format
	   "{%s,\n"
	   (or (alist-get "entry-id" values-alist nil nil #'string=)
	       "<entry-id>"))))
	(make-field
	 #'(lambda
	     (field opt)
	     (let ((value
		    (alist-get
		     (car field)
		     values-alist
		     nil nil #'string=)))
	       (library-make-single-bibtex-field field opt value)))))
    (concat
     (substring
      (apply
       'concat
       (append (list entry)
	       (mapcar
		#'(lambda (field) (funcall make-field field nil))
		(car field-list))
	       (mapcar
		#'(lambda (field) (funcall make-field field 'optional))
		(cdr field-list))))
      0 -2)
     "\n}\n")))

(defun library-capture-template (entry-type category)
  "Generate a capture template based on ENTRY-TYPE.

CATEGORY is the category to be placed in the `:CATEGORY:' field
in the properties drawer of the entry.  See
`library-bibtex-entry' for information on ENTRY-TYPE.

This function is interactive and requires input of values for
authors, title, and year of publication."
  (let (first-author
	last-read-author other-authors
	year title entry-id)
    (setq
     first-author
     (read-from-minibuffer "First author: ")
     other-authors
     (or (while (not (string-empty-p last-read-author))
	   (setq last-read-author
		 (read-from-minibuffer "Next author (leave blank to finish): "))
	   (setq other-authors (cons last-read-author other-authors)))
	 (reverse (cdr other-authors)))
     year
     (read-from-minibuffer "Year of publication: ")
     title
     (read-from-minibuffer "Title: ")
     entry-id
     (if (and (not (string-empty-p first-author))
	      (not (string-empty-p year)))
	 (concat
	  (downcase (car (last (split-string first-author))))
	  year)))
    (eval
     `(concat
       "* "
       (or entry-id "<entry id>")
       ": %?"
       (if (string-empty-p title) "Title" title)
       " :keywords:
:PROPERTIES:
:ID: "
       (or entry-id "<entry id>")
       (format "%s%s" "\n:CATEGORY: " category)
       "\n:END:
- Added: %t
- Year: "
       (if (not (string-empty-p year)) year)
       "\n- Author"
       ,@(if (not other-authors)
	     (list (format ": %s" first-author))
	   (cons (format "s:\n  - %s" first-author)
		 (mapcar
		  #'(lambda (author)
		      (format "\n  - %s" author))
		  other-authors)))
       "\n\n** Resources
- [[https://]]
- [[file:]]

** Citation
#+begin_src bibtex\n"
       (library-bibtex-entry
	entry-type
	(list
	 (when entry-id
	   (cons "entry-id" entry-id))
	 (if (not (string-empty-p year))
	     (cons "year" year))
	 (if (not (string-empty-p title))
	     (cons "title" title))
	 (cons "author"
	       (if (not other-authors)
		   first-author
		 (concat
		  first-author
		  ,@(mapcar
		     #'(lambda (author)
			 (format " and %s" author))
		     other-authors))))))
       "#+end_src

** TODO Summary
TBD."))))

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
    #'(lambda ()
	(library-capture-template
	 ,entry-type
	 ,(car (split-string description))))
    :jump-to-captured t
    ))

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

(defun library-add-entry (&optional goto keys)
  "Add new entry to the library.

GOTO and KEYS are passed to `org-capture'.  See its help for more
information."
  (interactive "P")
  (require 'org-capture)
  (bibtex-set-dialect)
  (with-current-buffer
      (find-file-noselect library-org-file 'nowarn)
    (revert-buffer nil 'noconfirm)
    (read-only-mode -1))
  (let ((org-capture-templates
	 `(,(library-generate-capture-template
	     "b" "book"
	     "Book" "Books and theses")
	   ,(library-generate-capture-template
	     "c" "inproceedings"
	     "Conference paper" "Conference papers")
	   ,(library-generate-capture-template
	     "j" "article"
	     "Journal paper" "Journal papers")
	   ,(library-generate-capture-template
	     "t" "phdthesis"
	     "Thesis" "Books and theses")
	   ,(library-generate-capture-template
	     "o" "booklet"
	     "Other" "Other references"))))
    (org-capture goto keys)))

;; Set up global key bindings for the library
(global-set-key (kbd "C-c l o") 'library-find-org-file)
(global-set-key (kbd "C-c l s") 'library-search)
(global-set-key (kbd "C-c l b") 'library-find-bib-file)
(global-set-key (kbd "C-c l a") 'library-add-entry)
(global-set-key (kbd "C-c l t") 'library-tangle-file)
(global-set-key (kbd "C-c l r") 'library-dired-resources-directory)
