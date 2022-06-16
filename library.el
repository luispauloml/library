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

(defun library-search ()
  "Find `library-org-file' in Read-Only mode and start incremental
search forward."
  (interactive)
  (library-find-org-file)
  (isearch-forward))

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
	(entry (concat "@" entry-type "{bibtex-entry,\n")))
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

(defun library-capture-template (entry-type)
  "Generate a capture template based on ENTRY-TYPE.  See
`library-bibtex-entry' for information on ENTRY-TYPE."
  (concat
   "* Title%? <<bibtex-key>>
- Year: 
- Author: 

** Keywords :keyword1:keyword1:

** TODO Summary
TBD.

Resources:
- [[https://]]
- [[file:]]

Added: %t

** Citation
#+begin_src bibtex\n"
   (library-bibtex-entry entry-type)
   "#+end_src"))

(defun library-generate-capture-template (key entry-type description headline)
  "Generate a template for a library entry to be used in `org-capture'.
KEY is the key to select the type of entry in the selection
buffer; ENTRY-KEY is the corresponding BibTeX key for the entry;
DESCRIPTION is the description to that key in that frame;
HEADLINE is the heading under which it will appear in the
library's org file."
  `(,key
    ,description
    entry
    (file+headline
     ,(concat current-user-directory "library/library.org")
     ,headline)
    ,(library-capture-template entry-type)
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
  (save-excursion
    (cond
     ((and (search-forward "begin_src bibtex" nil t)
	   (string= (buffer-name (current-buffer))
		    (concat "CAPTURE-"
			    (file-name-nondirectory library-org-file))))
      (next-line)
      (ignore-error nil (bibtex-clean-entry))))))

(add-hook 'org-capture-before-finalize-hook
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
