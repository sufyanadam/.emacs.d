(defalias 'λ 'lambda)

;; shorthand for interactive lambdas
(defmacro Λ (&rest body)
  `(λ ()
     (interactive)
     ,@body))


(provide 'real-lambda)
