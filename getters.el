
(defun cria-getters (ini fim)
  "Cria getters e setters de atributos declarados entre INI e FIM"
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
		     (concat "\npublic void set" (upcase (match-string 4)) (match-string 5) "(" (match-string 2) " " (match-string 3) ") {\n"
			     "this." (match-string 3) " = " (match-string 3) ";\n}\n")
		   "\n")
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

(provide 'cria-getters)
