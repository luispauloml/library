(defun library-extract-find-bibtex-entry ()
  "Find a BibTeX entry in current buffer."
  (require 'reftex-parse)
  (when (search-forward "bibcite" nil 'noerror)
    (when (looking-at-p "{")
      (forward-char)
      (when (reftex-what-macro-safe (list "\\bibcite"))
	(reftex-this-word "^{}%\n\r, \t")))))

(defun library-extract-aux-entries ()
  "Extract BibTeX entries from an AUX file.

Note: It interactively asks for the AUX file."
  (let* ((file-name
	  (read-file-name "Choose AUX file: " nil default-directory t))
	 (buffer (find-file-noselect file-name))
	 entries found-pos)
    (with-current-buffer buffer
      (save-excursion
	(goto-char 0)
	(while (setq found-pos (library-extract-find-bibtex-entry))
	  (setq entries (append entries (list found-pos))))))
    entries))

(defun library-extract-bibtex-entry-org-node-p (s n)
  "Return t if S is the id of org-roam node N."
  (string= s (org-roam-node-id n)))

(defun library-extract-copy-entries-from-org ()
  "Copy BibTeX entries from org-roam.

Make copies of the org-roam nodes related to entries extracted by
`library-extract-aux-entries'."
  (require 'org-roam)
  (let* ((entries (library-extract-aux-entries))
	 (node-list (org-roam-node-list))
	 (get-node-pos
	  #'(lambda (k)
	      (cl-position
	       k node-list
	       :test 'library-extract-bibtex-entry-org-node-p)))
	 selected-nodes this-entry)
    (while entries
      (setq this-entry (funcall get-node-pos (pop entries)))
      (if this-entry
	  (setq selected-nodes
		(cons (nth this-entry node-list) selected-nodes))))
    selected-nodes))

(defun library-extract-bibtex-entries ()
  "Extract BibTeX entries from `library-org-file' base on entries
listed in an AUX file.

It interactively asks for the path to the AUX file."
  (interactive)
  (save-excursion
    (insert
     (with-temp-buffer
       (let ((sn (library-extract-copy-entries-from-org)))
	 (while sn
	   (insert
	    (org-element-property
	     :value
	     (with-current-buffer (find-file-noselect library-org-file)
	       (let* ((found nil)
		      (curr-pos (org-roam-node-point (pop sn)))
		      (elt (prog2 (goto-char curr-pos) (org-element-at-point)))
		      (bound (org-element-property :contents-end elt)))
		 (while (and (not found) (< curr-pos bound))
		   (setq elt (org-element-at-point))
		   (if (string= (plist-get (cadr elt) :raw-value) "Citation")
		       (setq found elt))
		   (setq curr-pos (prog2 (org-next-visible-heading 1) (point))))
		 (goto-char (org-element-property :contents-begin elt))
		 (setq elt (org-element-at-point)))))
	    "\n"))
	 (substring (buffer-string) 0 -1))))))
