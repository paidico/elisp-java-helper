;; java junit5 test template

(defun djunit-new-file (classFile)
  "Cria template de teste no caminho CLASSFILE e retorna arquivo criado"
  (let ((classTestDir (replace-regexp-in-string "^\\(.+\\)/src/main/java/\\([a-z/]+\\)/\\([A-Z][A-Za-z]+\\)\\.java"
						"\\1/src/test/java/\\2"
						classFile))
	(classTestFile (replace-regexp-in-string "^\\(.+\\)/src/main/java/\\([a-z/]+\\)/\\([A-Z][A-Za-z]+\\)\\.java"
						 "\\1/src/test/java/\\2/\\3Test.java"
						 classFile))
	(classUnderTest (replace-regexp-in-string "^\\(.+\\)/src/main/java/\\([a-z/]+\\)/\\([A-Z][A-Za-z]+\\)\\.java"
						  "\\3" classFile))
	(classPackage (replace-regexp-in-string "/" "."
		      				(replace-regexp-in-string
						 "^\\(.+\\)/src/main/java/\\([a-z/]+\\)/\\([A-Z][A-Za-z]+\\)\\.java"
						 "\\2" classFile))))
    (progn
      (if (not (file-exists-p classTestFile))
	  (progn
	    (if (not (file-exists-p classTestDir))
		(make-directory classTestDir t))
	    (with-temp-file
		classTestFile
	      (insert (concat "package " classPackage			  
			      ";\n\nimport static org.junit.jupiter.api.Assertions.fail;\n\n"
			      "import org.junit.jupiter.api.BeforeEach;\nimport org.junit.jupiter.api.Test;\n\n"
			      "class " classUnderTest "Test {\n\n@BeforeEach\nvoid setUp() throws Exception {\n}\n\n"
			      "@Test\nvoid test() {\nfail(\"not implemented\");\n}\n}\n"
			      )
		      ))
	    ))
      )
    classTestFile)
  )

(defun djunit ()
  "Cria template de teste se for um arquivo java"
  (interactive)
  (progn
    (save-excursion
      (if (string-match-p "^\\(.+\\)/src/main/java/\\([a-z/]+\\)/\\([A-Z][A-Za-z]+\\)\\.java"
			  (buffer-file-name))
	  (progn
	    (find-file (djunit-new-file (buffer-file-name)))
	    (indent-region (point-min) (point-max))
	    (save-buffer)
	    ))
      )
    )
  )

(provide 'djunit)
