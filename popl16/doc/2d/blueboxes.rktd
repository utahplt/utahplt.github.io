200
((3) 0 () 0 () () (h ! (equal) ((q form ((lib "2d/tabular.rkt") 2dtabular)) q (1961 . 26)) ((q form ((lib "2d/cond.rkt") 2dcond)) q (0 . 33)) ((q form ((lib "2d/match.rkt") 2dmatch)) q (1127 . 22))))
syntax
(2dcond cond-content)
 
 cond-content = question-row
                body-row
                ⋮
              | question-row
                body-row
                ⋮
                else-row
                 
 question-row = empty-cell question-cell ⋯
              | empty-cell question-cell ⋯ else-cell
                 
     body-row = question-cell exprs-cell ⋯
                 
     else-row = question-cell exprs-cell ⋯ else-cell
                 
question-cell = ╔═════════════╗
                ║question-expr║
                ╚═════════════╝
                 
   empty-cell = ╔═══╗
                ║   ║
                ╚═══╝
                 
   exprs-cell = ╔═════════════╗
                ║expr expr ...║
                ╚═════════════╝
                 
    else-cell = ╔══════╗
                ║ else ║
                ╚══════╝
syntax
(2dmatch match-content)
 
  match-content = match-first-row
                  match-row
                  ⋮
                   
match-first-row = two-expr-cell match-pat-cell ⋯
                   
      match-row = match-pat-cell exprs-cell ⋯
                   
  two-expr-cell = ╔═════════════════╗
                  ║col-expr row-expr║
                  ╚═════════════════╝
                   
 match-pat-cell = ╔═════╗
                  ║ pat ║
                  ╚═════╝
                   
     exprs-cell = ╔═════════════╗
                  ║expr expr ...║
                  ╚═════════════╝
syntax
(2dtabular tabular-content)
 
tabular-content = tabular-row
                  ⋮
                | tabular-row
                  ⋮
                  style-cell
                   
    tabular-row = tabular-cell ⋯
                   
   tabular-cell = ╔════════════════╗
                  ║tabular-expr ...║
                  ╚════════════════╝
                   
     style-cell = ╔═════════════════╗
                  ║style-content ...║
                  ╚═════════════════╝
                   
  style-content = #:style style-expr
                | #:sep sep-expr
                | #:ignore-first-row
 
  style-expr : style?
  sep-expr : (or/c block? content? #f)
  tabular-expr : (or/c block? content?)
