(module code mzscheme
  (require (lib "code.ss" "slideshow"))

  (current-keyword-list '("define" "cond" "define-struct" "and" "or" "else" "local" "lambda" 
			  "set!" "begin"))
  (current-const-list '("true" "false" "empty"))

  (provide (all-from (lib "code.ss" "slideshow"))))
