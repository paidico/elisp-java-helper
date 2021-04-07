(defun cria-javadoc (text &optional ret &rest prms)
  "Cria template de javadoc"
  (let (
	(jdoc (list))
	)
    (progn 
      (push "/**"
	    jdoc)
      (push (concat "* " text ".")
	    jdoc)
      (dolist (p prms)
	(push (concat "* @param " p " " p)
	      jdoc))
      (if ret
	  (push (concat "* @return " ret)
		jdoc)
	)
      (push "*/\n"
	    jdoc)
      (mapconcat 'identity (reverse jdoc) "\n")
      ))
  )
(defun cria-javadoc-attr (ini fim)
  "Cria template de javadoc para atributos entre INI e FIM"
  (interactive "*r")
  (let ((gsttrs (list))
	(ini (copy-marker (min ini fim)))
	(fim (copy-marker (max ini fim))))
    (progn
      (save-excursion
	(goto-char ini)
	(while
	    (re-search-forward
	     "^\\s-*\\(?:private\\|protected\\)?\\(?:\\s-+\\(?:final\\|transient\\|volatile\\|static\\)\\)*\\s-+\\S-+\\(?:<[^>]+>\\)?\\s-+\\([a-zA-Z0-9_]+\\)\\s-*\\(?:;\\|\\(?:=.*\\)?\\)\\s-*$"
	     fim t)
	  (replace-match (concat (cria-javadoc (match-string 1))
				 (match-string 0)))
	  )
	(indent-region (point-min) (point-max))
	))
    )
  )
(defun cria-getters-sun (ini fim)
  "Cria getters e setters sun-checked de atributos entre INI e FIM"
  (interactive "*r")
  (let (
	(gsttrs (list))
	(ini (copy-marker (min ini fim)))
	(fim (copy-marker (max ini fim)))
	)
    (save-excursion
      (goto-char ini)
      (while
	  (re-search-forward
	   "^\\s-*private\\(\\s-+\\(?:final\\|transient\\|volatile\\)\\)?\\s-+\\(\\S-+\\(?:<[^>]+>\\)?\\)\\s-+\\(\\([a-zA-Z_]\\)\\([a-zA-Z0-9_]+\\)\\)\\s-*\\(?:;\\|\\(?:=.*\\)?\\)\\s-*$"
	   fim t)
	(push
	 (concat (if
		     (or (not (match-string 1))
			 (not (string=
			       (replace-regexp-in-string "^\\s-*\\(\\S-*.*\\S-*\\)\\s-*$" "\\1" (match-string 1))
			       "final"))
			 )
		     (concat (cria-javadoc (concat "Setter for " (match-string 3)) nil (match-string 3))
			     "public void set" (upcase (match-string 4)) (match-string 5) "(final " (match-string 2) " " (match-string 3) ") {\n"
			     "this." (match-string 3) " = " (match-string 3) ";\n}\n")
		   "\n")
		 (cria-javadoc (concat "Getter for " (match-string 3)) (match-string 3))
		 "public " (match-string 2) " " (if (string= "boolean" (match-string 2)) "is" "get") (upcase (match-string 4)) (match-string 5) "() {\n"
		 "return " (match-string 3) ";\n}\n"
		 )
	 gsttrs))
      (goto-char fim)
      (newline)
      (dolist (gs (reverse gsttrs))
	(insert gs))
      (indent-region fim (point))
      ))
  )

(provide 'cria-getters-sun)
(provide 'cria-javadoc-attr)
