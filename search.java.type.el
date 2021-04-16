
(defun redefine-find-cmd (find-compl position)
  "Redefine find default com sufixo FIND-COMPL e POSITION"
  (let ((cur-grep-find-command grep-find-command)
	(counter-grep-find-command 0)
	(cur-dir default-directory)
	(search-dir "~"))
    (progn
      (setq search-dir (substring cur-dir 0 (s-index-of "/test/" cur-dir)))
      (setq search-dir (substring search-dir 0 (s-index-of "/main/" search-dir)))
      (setq cur-grep-find-command
	    (s-concat
	     "find "
	     search-dir
	     find-compl))
      (setq counter-grep-find-command
	    (+ (length search-dir) position))
      (grep-compute-defaults)
      ;; replacing default find command
      (grep-apply-setting
       'grep-find-command
       (cons cur-grep-find-command counter-grep-find-command))
      )
    ))

(defun busca-java-type (command-args)
  "Busca tipos java por COMMAND-ARGS"
    (interactive
     (progn
       (redefine-find-cmd
	" -name '*.java' -type f -exec grep --color -nH --null -E '\\b(class|interface|enum)\\s+' \\{\\} +"
	91)
       (list (read-shell-command "Run: "
				 grep-find-command 'grep-find-history))
       ))
    (when command-args
      (let ((null-device nil))
	(grep command-args)))
    (grep-apply-setting
     'grep-find-command
     (cons "find . -type f -exec grep --color -nHZ -e  \\{\\} +" 43))
    )

(defun busca-java-extensions (command-args)
  "Busca tipos java herdados por COMMAND-ARGS"
    (interactive
     (progn
       (redefine-find-cmd
	" -name '*.java' -type f -exec grep -ZzlP '\\b(extends|implements)\\b[^{]+\\b\\b.*{' \\{\\} + | sed 's/\\x0/:1:found\\n/g'"
	79)
       (list (read-shell-command "Run: "
				 grep-find-command 'grep-find-history))
       ))
    (when command-args
      (let ((null-device nil))
	(grep command-args)))
    (grep-apply-setting
     'grep-find-command
     (cons "find . -type f -exec grep --color -nHZ -e  \\{\\} +" 43))
    )

(provide 'busca-java-type)
(provide 'busca-java-extensions)
