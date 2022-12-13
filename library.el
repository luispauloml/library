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

(defvar library-last-entry-resource-file-alist nil)

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

(defun library-get-resource-file ()
  (interactive)
  (cond
   ((and (buffer-file-name)
	 (yes-or-no-p
	  (format
	   "Use current file as resource [%s]?"
	   (file-name-nondirectory (buffer-file-name)))))
    (buffer-file-name))
   ((eq major-mode 'dired-mode)
    (let ((file-name
	   (or (dired-get-marked-files nil nil)
	       (dired-get-marked-files nil t))))
      (if (= 1 (length file-name))
	  (setq file-name (car file-name)))
      (when (yes-or-no-p
	     (format
	      "%s [%s]?"
	      "Use selected file as resource"
	      (if (listp file-name)
		  (format "%i files selected" (length file-name))
		(file-name-nondirectory file-name))))
	(if (listp file-name)
	    (setq file-name
		  (completing-read
		   "Choose a file: "
		   file-name)))
	file-name)))))

(defun library-new-resource-file (newname)
  (let ((file-name (file-name-concat library-resources-directory newname)))
    (cond
     ((file-exists-p file-name)
      (library-new-resource-file
       (completing-read
	(format
	 "\"%s\" already exists. Choose a different name: "
	 newname)
	(list newname))))
     (t
      newname))))

(defun library-capture-template (entry-type category)
  "Generate a capture template based on ENTRY-TYPE.

CATEGORY is the category to be placed in the `:CATEGORY:' field
in the properties drawer of the entry.  See
`library-bibtex-entry' for information on ENTRY-TYPE.

This function is interactive and requires input of values for
authors, title, and year of publication."
  (let (file-name resource-file
	first-author
	last-read-author other-authors
	year title entry-id)
    (setq
     file-name
     (library-get-resource-file)
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
	  year))
    resource-file
    (cond
     ((and file-name entry-id)
      (library-new-resource-file
       (format "%s.%s" entry-id (file-name-extension file-name))))
     ((and file-name (not entry-id))
      (library-new-resource-file
       (completing-read "Choose new name of the file: "
			`(,(file-name-nondirectory file-name)))))))
    (if (and file-name resource-file)
	(setq library-last-entry-resource-file-alist
	      `((file . ,file-name)
		(newname . ,(file-name-concat
			     library-resources-directory
			     resource-file)))))
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
       "\n\n** TODO Summary
TBD.

** Resources
- [[https://]]
- [[file:"
       (format
	"%s]]\n\n"
	(if resource-file
	    (concat "./resources/" resource-file)
	  ""))
       "** Citation
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
       "#+end_src"))))

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

(defun library-maybe-copy-resource-file ()
  (when library-last-entry-resource-file-alist
    (copy-file
     (cdr (assoc 'file library-last-entry-resource-file-alist))
     (cdr (assoc 'newname library-last-entry-resource-file-alist))
     1))
  (setq library-last-entry-resource-file-alist nil))

(add-hook 'org-capture-prepare-finalize-hook
	  'library-bibtex-clean-entry)
(add-hook 'org-capture-prepare-finalize-hook
	  'library-maybe-copy-resource-file)

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

;;; library.el ends here
