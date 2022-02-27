
(defun djson-format-text-tokenize (ini fim)
  "Tokeniza para valores entre aspas de INI ate FIM"
  (let ((tokens (list)))
    (goto-char ini)
    (while (search-forward "\\\"" fim t)
      (replace-match "#_quote-tokenized_#"))
    (goto-char ini)
    (while (re-search-forward
	    "\\(\"[^\"]+\"\\)\\s-*:\\s-*\\(\"[^\"]*\"\\)"
	    fim t)
      (let* ((orig-key (match-string 1))
	     (orig-value (match-string 2)))
	(replace-match (concat orig-key ": #_text-tokenized_#"))
	(push orig-value tokens)
	))
    (reverse tokens))
  )

(defun djson-format-text-detokenize (tokens ini fim)
  "Substitui TOKENS de valores entre aspas de INI ate FIM"
  (progn
    (goto-char ini)
    (while (search-forward "#_text-tokenized_#" fim t)
      (let* ((orig-value (pop tokens)))
	(replace-match orig-value)
	))
    (goto-char ini)
    (while (search-forward "#_quote-tokenized_#" fim t)
      (replace-match "\\\\\""))
    )
  )

(defun djson-format-remove-break-ln (ini fim)
  "Limpa quebra de linhas e tabs entre INI e FIM"
  (progn
    (goto-char ini)
    (while (re-search-forward "\\s-*\n\\s-*" fim t)
      (replace-match ""))
    (untabify ini fim)
    )
  )

(defun djson-format-square-brackets (ini fim)
  "Formata colchetes entre INI e FIM"
  (progn
    (goto-char ini)
    (while (re-search-forward "\\s-*[[]\\s-*" fim t)
      (replace-match "[\n"))
    (goto-char ini)
    (while (re-search-forward "\\s-*[\]]\\s-*" fim t)
      (replace-match "\n]"))
    (goto-char ini)
    (while (re-search-forward "\\s-*[[]\\s-*\\(\n\\s-*\\)+[\]]" fim t)
      (replace-match "[]"))
    )
  )

(defun djson-format-curly-brackets (ini fim)
  "Formata chaves entre INI e FIM"
  (progn
    (goto-char ini)
    (while (re-search-forward "\\s-*{\\s-*" fim t)
      (replace-match "{\n"))
    (goto-char ini)
    (while (re-search-forward "\\s-*}\\s-*" fim t)
      (replace-match "\n}"))
    (goto-char ini)
    (while (re-search-forward "\\s-*{\\s-*\\(?:\n\\s-*\\)+}" fim t)
      (replace-match "{}"))
    )
  )

(defun djson-format-comma (ini fim)
  "Formata virgula entre INI e FIM"
  (progn
    (goto-char ini)
    (while (re-search-forward "\\s-*,\\s-*" fim t)
      (replace-match ",\n"))
    )
  )

(defun djson-format-colon (ini fim)
  "Formata dois pontos entre INI e FIM"
  (progn
    (goto-char ini)
    (while (re-search-forward "\\s-*:\\s-*" fim t)
      (replace-match ": "))
    )
  )

(defun djson-format (&optional ini fim)
  "Formata Json entre INI e FIM"
  (interactive "*r")
  (let ((ini-mark (copy-marker (point-min)))
	(fim-mark (copy-marker (point-max)))
	(tokens (list)))
    (save-excursion
      (if (and ini fim)
	  (progn
	    (setq ini-mark (copy-marker (min ini fim)))
	    (setq fim-mark (copy-marker (max ini fim)))
	    ))
      (djson-format-remove-break-ln ini-mark fim-mark)
      (setq tokens (djson-format-text-tokenize ini-mark fim-mark))
      (djson-format-square-brackets ini-mark fim-mark)
      (djson-format-curly-brackets ini-mark fim-mark)
      (djson-format-comma ini-mark fim-mark)
      (djson-format-colon ini-mark fim-mark)
      (djson-format-text-detokenize tokens ini-mark fim-mark)
      (indent-region ini-mark fim-mark)
      (untabify ini-mark fim-mark)
      ))
  )

(defun djson-unformat (&optional ini fim)
  "Remove formatacao de Json entre INI e FIM"
  (interactive "*r")
  (let ((ini-mark (copy-marker (point-min)))
	(fim-mark (copy-marker (point-max))))
    (save-excursion
      (if (and ini fim)
	  (progn
	    (setq ini-mark (copy-marker (min ini fim)))
	    (setq fim-mark (copy-marker (max ini fim)))
	    ))
      (djson-format-remove-break-ln ini-mark fim-mark)
      (indent-region ini-mark fim-mark)
      ))
  )

(defun djson-collapse-next ()
  "Remove formatacao de Json dentro do proximo agrupador partindo da posicao atual"
  (interactive)
  (let ((start-mark)
	(start-symbol)
	(end-symbol)
	(square-bracket-open-symbol (string-to-char "["))
	(curly-bracket-open-symbol (string-to-char "{"))
	(square-bracket-close-symbol (string-to-char "]"))
	(curly-bracket-close-symbol (string-to-char "}"))
	(opening-symbols)
	(closing-symbols)
	(tokens (list)))
    (save-excursion
      (setq opening-symbols (list square-bracket-open-symbol curly-bracket-open-symbol))
      (setq closing-symbols (list square-bracket-close-symbol curly-bracket-close-symbol))
      (save-excursion
	(setq tokens (djson-format-text-tokenize (point-min) (point-max))))

      (while (not (member (char-after) opening-symbols))
	(forward-char))
      (setq start-symbol (char-after))
      (if (member start-symbol opening-symbols)
	  (let* ((symbol-count 1))
	    (if (eq start-symbol square-bracket-open-symbol)
		(setq end-symbol square-bracket-close-symbol)
	      (setq end-symbol curly-bracket-close-symbol))
	    (setq start-mark (point))
	    (forward-char)
	    (while (and (not (eobp)) (> symbol-count 0))
	      (let* ((actual-symbol (char-after)))
		(forward-char)
		(if (eq actual-symbol end-symbol)
		    (setq symbol-count (- symbol-count 1)))
		(if (eq actual-symbol start-symbol)
		    (setq symbol-count (+ symbol-count 1)))
		))
	    (if (eq (char-before) end-symbol)
		(djson-unformat start-mark (point)))
	    )
	)
      (djson-format-text-detokenize tokens (point-min) (point-max))
      )
    )
  )

(provide 'djson-format)
(provide 'djson-unformat)
(provide 'djson-collapse-next)
