;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java-helper.el		 ;;
;; v0.0.1			 ;;
;; Remove imports não utilizados ;;
;; Cria getters e setters	 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cria-gsttrs-region (ini fim)
  "Cria getters e setters de atributos declarados entre INI e FIM"
  (interactive "*r")
  (let ((gsttrs (list))
	(fim (copy-marker fim)))
    (save-excursion
      (while 
	  (re-search-forward 
	   "^\\s-*private\\s-+\\(\\S-+\\(?:<.*>\\)?\\)\\s-+\\([a-zA-Z0-9_]+\\)\\s-*;\\s-*$" 
	   fim t)
	(push  
	 (concat "\npublic void set"
		 (upcase (substring (match-string 2) 0 1))
		 (substring (match-string 2) 1) "("
		 (match-string 1) " value) {\n\t"
		 (match-string 2) " = value;\n}\npublic "
		 (match-string 1) " "
		 (if (string= "boolean" (match-string 1))
		     "is"
		   "get")
		 (upcase (substring (match-string 2) 0 1))
		 (substring (match-string 2) 1) "() {\n\t return "
		 (match-string 2) ";\n}\n")
	 gsttrs))
      (goto-char fim)
      (newline)
      (dolist (gs (reverse gsttrs))
	(insert gs)))))

(defun remove-lindups-region (ini fim)
  "Encontra linhas duplicadas entre INI e FIM e mantém a primeira ocorrência"
  (save-excursion
    (while
	(progn
	  (goto-char ini)
	  (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" fim t))
      (replace-match "\\1\n\\2"))))

(defun limpa-imports ()
  "Remove imports não utilizados"
  (interactive)
  (let ((imports (list))
	(unused-imports (list))
	(b0 (point-min))
	(b1 (point-max))
	(reinit "^\\s-*package\\s-+\\(?:[a-z0-9_]+\\.\\)*[a-z0-9_]+\\s-*;")
	(reimport "^\\s-*\\(import\\(?:\\s-+static\\)?\\s-+\\(?:[a-zA-Z0-9_]+\\.\\)*\\([a-zA-Z0-9_]+\\)\\s-*;\\)\\s-*$")
	(reend "^\\s-*p\\(?:ublic\\|rotected\\|rivate\\)?\\(?:\\s-+[a-z]+\\)*\\s-+\\(?:class\\|interface\\).*$"))
    (save-excursion
      (setq case-fold-search nil)
      (goto-char b0)
      ;; movendo para início do bloco de import 	
      (re-search-forward reinit nil t)
      (setq b0 (point))
      ;; marcando fim do bloco
      (if
	  (re-search-forward reend nil t)
	  (setq b1 (match-beginning 0)))
      (goto-char b0)
      (while
	  (re-search-forward reimport b1 t)
	(push (cons (match-string 2) (match-string 1)) imports))
      ;; loop through alist searching in the remaining file, \bImport\b ...
      (while imports
	(unless
	    (re-search-forward (format "\\b%s\\b" (car (car imports))) nil t)
	  (push (cdr (car imports)) unused-imports))
	(goto-char b1)
	(setq imports (cdr imports)))
      (goto-char b0)
      (while unused-imports
	(if (search-forward (car unused-imports) b1 t)
	    (replace-match ""))
	(setq unused-imports (cdr unused-imports)))
      (sort-lines nil b0 b1)
      (remove-lindups-region b0 b1)
      ;; linha adicional entre bloco de imports e definição da classe
      (if
	  (re-search-forward reend nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (newline))))))
