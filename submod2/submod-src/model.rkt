#lang racket
(require redex/reduction-semantics
         "syntax-rule.rkt")

(provide (all-defined-out))

;; Grammar ----------------------------------------

(define-language L
  ;; Code starts as an S-expression:
  [s-exp nam (s-exp ...)]

  ;; "Nam" is short for "name":
  [nam desc-name] ; `desc-name' typesets as prose
  [desc-name variable-not-otherwise-mentioned]
  
  ;; A compiled module has a name, dependencies, and definitions:
  [mod (Module mnam (Seq dep ...) (Seq defn ...))]
  [M (Seq mod ...)]
  [mnam nam]
  
  ;; Each dependency pairs a phase with a module name:
  [dep [At mnam ph]]
  [deps (Seq dep ...)]
  
  ;; A module contains value and macro definitions at various phases:
  [defn (Defn dnam ph kind ast)]
  [defns (Seq defn ...)]
  [dnam nam]
  [kind Value Macro]

  ;; A phase is a number --- 0 for run-time, 1 for compile time, etc.:
  [ph number]
  
  ;; The right-hand side of a definition is a compiled expression,
  ;; i.e., an AST:
  [ast val
       (Ref binding) ; reference to a module's definition
       var ; reference to a `lambda' argument
       (App ast ast ...)]
  [val func
       (Quote nam)
       (Syntax stx)
       Unbound] ; reflect an unbound-identifier error
  [func (Lambda (Seq var ...) ast)]
  [var (Var nam)]
  
  ;; An S-expression enriched with binding information is a syntax
  ;; object, stx:
  [stx id (stx ...)]
  [id (Stx nam bindings)]
  
  ;; Each binding has a phase, module name, and defined name:
  [binding (Bind ph mnam dnam)]
  [bindings (Seq binding ...)]
  ;; A binding-resolution result might be #f:
  [maybe-binding binding
                 #f]

  ;; During a module compilation, the body yet to be compiled is 
  ;; maintained as a list of `body's that each pairs a syntax object
  ;; with the phase of the form.
  [body [At stx ph]]
  
  ;; An environment is used for `lambda' arguments during compilation
  ;; of an expression:
  [env (Seq var ...)]
  
  ;; A store holds instantiated module bindings (at both compile time
  ;; and run time):
  [Σ (Sto cell ...)]
  [cell (Cell binding kind val)
        (Cell [At mnam ph])]

  [ready-binding [At mnam ph]]

  ;; Store lookup succeeds with a kind and value, or it fails:
  [lookup-result [Tup kind val]
                 #f]
  [ready-lookup-result Ready
                       #f]

  ;; For overloading `shift':
  [stx-or-ast stx ast])

;; Examples ----------------------------------------

(module+ test
  (require rackunit)

  ;; Helper macro to make the examples pretty:
  (define-syntax !
    (syntax-rules (=>)
      [(_ mod ... => result)
       (check-equal? (term (run/main-result mod ...))
                     (term result))]))
  
  
  ;; The simplest possible module:
  (! (module main base
       (define result 'ok))
     =>
     (Quote ok))

  ;; A macro definition and use:
  (! (module main base
       (require (for-syntax base))
       (define-syntax def-result
         (lambda (x)
           #'(define result 'gen-ok)))
       (def-result))
     =>
     (Quote gen-ok))

  ;; A module that defines a macro that is used in
  ;; another module:
  (! (module ok-definer base
       (require (for-syntax base))
       (define-syntax def-ok
         (lambda (x)
           #'(define ok 'is-ok))))
     (module main base
       (require ok-definer)
       (def-ok)
       (define result ok))
     =>
     (Quote is-ok))

  ;; A submodule declaration that is imported into its
  ;; enclosing module:
  (! (module main base
       (module sub base
         (define inner 'inside))
       (require sub)
       (define result inner))
     =>
     (Quote inside))

  ;; Declare a `module*' that imports from its enclosing module:
  (! (module main base
       (module sub base
         (define inner 'inside)
         (module* subsub base
           (require sub)
           (define innerinner inner)))
       (require subsub)
       (define result innerinner))
     =>
     (Quote inside))
  
  ;; Declare at `(module* ... #f ...)', but we write it in the
  ;; model as `(module* ... () ...)':
  (! (module main base
       (module sub base
         (define inner 'inside)
         (module* subsub ()
           (define innerinner inner)))
       (require subsub)
       (define result innerinner))
     =>
     (Quote inside))

  ;; Reference to an unbound identifier (which is a syntax error
  ;; in Racket, but is reflected as a run-time `Unbound' value
  ;; in the module):
  (! (module main base
       (define result nonesuch))
     =>
     Unbound)

  ;; Not imported, so not bound:
  (! (module other base
       (define elsewhere 'ok))
     (module main base
       (define result elsewhere))
     =>
     Unbound)

  ;; Ditto for submodule:
  (! (module main base
       (module sub base
         (define inner 'inside))
       (define result inner))
     =>
     Unbound)

  ;; Ditto for `module*':
  (! (module main base
       (module sub base
         (define inner 'inside)
         (module* subsub base
           (define innerinner inner)))
       (require subsub)
       (define result innerinner))
     =>
     Unbound)

  ;; The `other' module compiles, but it triggers an error if run.
  ;; Here, it isn't imported by `main', so it's not evaluated:
  (! (module other base
       (define bad (nonesuch nonesuch)))
     (module main base
       (define result 'ok))
     =>
     (Quote ok))

  ;; Running a submodule doesn't necessarily
  ;; run its encloding module:
  (! (module main base
       (module sub base
         (define inner (nonesuch nonesuch))
         (module subsub base
           (define innerinner 'inside2)))
       (require subsub)
       (define result innerinner))
     =>
     (Quote inside2))

  ;; A macro can expand to a submodule declaration and import:
  (! (module sub-definer base
       (require (for-syntax base))
       (define-syntax defsub
         (lambda (x)
           #'(begin
               (module sub base
                 (define inner 'inside))
               (require sub)))))
     (module main base
       (require sub-definer)
       (defsub)
       (define result inner))
     =>
     (Quote inside))

  ;; In a submodule, use a macro defined in the
  ;; enclosing module:
  (! (module main base
       (module sub base
         (require (for-syntax base))
         (define-syntax def-way-inside
           (lambda (x)
             #'(define way-inside 'inner!)))
         (module* subsub ()
           (def-way-inside)))
       (require subsub)
       (define result way-inside))
     =>
     (Quote inner!))

  ;; A submodule inside `begin-for-syntax':
  (! (module main base
       (module sub base
         (require (for-syntax base))
         (begin-for-syntax
          (define result-sym 'inner-result)
          (module* subsub ()
            (define get-result result-sym))))
       (require subsub)
       (define result get-result))
     =>
     (Quote inner-result)))

;; Compilation ----------------------------------------

;; cmodule
;;  Compiles one module from source, given a list of
;;  previously compiled modules that are available
;;  for import. Returns an extended list of compiled
;;  modules, including the given one and any submodules
;;  that it contains.
(define-metafunction L
  cmodule : s-exp M -> M
  [(cmodule (module mnam mnam_init s-exp ...) M)
   (cbody (Seq body ...) mnam  (Seq [At mnam_init 0]) (Seq) M Σ_init)
   (where Σ_init (visit mnam_init 0 M (Sto)))
   (where bindings (import mnam_init 0 M))
   (where (Seq body ...) (Seq [At (wrap s-exp bindings) 0] ...))])

(module+ test
  (check-equal? (term (cmodule (module m base (define x 'ex)) (Seq)))
                (term (Seq (Module m (Seq [At base 0])
                                   (Seq (Defn x 0 Value (Quote ex))))))))

;; cbody
;;  Walks through the sequence syntax objects of a module body, each
;;  annotated with the phase at which it should be compiled. All
;;  syntax objects start at phase 0, and the content of a `begin-for-syntax'
;;  is flattened into the sequence with an incremented phase.
;;  A store keeps instantiated module definitions, and compiling
;;  a `require' can visit a module (i.e., instantiate its compile-time
;;  definitions), which extends the store. A list of previously
;;  compiled modules (which are available for import) is given.
;;  Accumulate dependencies and definitions, and ultimately produce
;;  an extended list of compiled modules, which will include
;;  any submodules encountered in the module body.
(define-metafunction L
  cbody : (Seq body ...) mnam deps defns M Σ -> M
  ;; empty body sequence
  ;;  Done!
  [(cbody (Seq) mnam deps defns M Σ)
   (mods-extend M mnam (Module mnam deps defns))]
  ;; (begin ....)
  ;;  Flatten the enclosed forms and continue.
  [(cbody (Seq [At (id_beg stx ...) ph] body_rest ...) mnam deps defns M Σ)
   (cbody (Seq [At stx ph] ... body_rest ...) mnam deps defns M Σ)
   (where (Bind ph base begin) (resolve id_beg ph))]
  ;; (begin-for-syntax ....)
  ;;  Shift the phase of the enclosed forms and continue.
  [(cbody (Seq [At (id_bfs stx ...) ph] body_rest ...) mnam deps defns M Σ)
   (cbody (Seq [At stx (p+ ph 1)] ... body_rest ...) mnam deps defns M Σ)
   (where (Bind ph base begin-for-syntax) (resolve id_bfs ph))]
  ;; (define ....)
  ;;  Compile the definition's right-hand side, and enrich the
  ;;  lexical context of the rest of the module. (Each definition
  ;;  acts like `let*', binding only in the rest of the module.)
  [(cbody (Seq [At (id_def id_new stx) ph] body_rest ...) mnam deps (Seq defn ...) M Σ)
   (cbody (Seq body_new ...) mnam deps (Seq defn ... defn_new) M Σ_new)
   (where (Bind ph base define) (resolve id_def ph))
   (where dnam (strip id_new))
   (where ast (cexpr stx ph (Seq) Σ))
   (where defn_new (Defn dnam ph Value ast))
   (where binding_new (Bind ph mnam dnam))
   (where (Seq body_new ...) 
          (Seq (add body_rest (Seq binding_new)) ...))
   (where val (maybe-eval ph ast Σ))
   (where Σ_new (extend Σ binding_new [Tup Value val]))]
  ;; (define-syntax ....)
  ;;  Like `define', but compile at one phase higher, and always
  ;;  evaluate immediately so the macro is available for use later
  ;;  in the body.
  [(cbody (Seq [At (id_defstx id_new stx) ph] body_rest ...) mnam deps (Seq defn ...) M Σ)
   (cbody (Seq body_new ...) mnam deps (Seq defn ... defn_new) M Σ_new)
   (where (Bind ph base define-syntax) (resolve id_defstx ph))
   (where dnam (strip id_new))
   (where ast (cexpr stx (p+ ph 1) (Seq) Σ))
   (where defn_new (Defn dnam ph Macro ast))
   (where binding_new (Bind ph mnam dnam))
   (where (Seq body_new ...)
          (Seq (add body_rest (Seq binding_new)) ...))
   (where val (eval ast Σ))
   (where Σ_new (extend Σ binding_new [Tup Macro val]))]
  ;; (macro-id ....)
  ;;  Expand a macro.
  [(cbody (Seq [At (id_macro stx ...) ph] body_rest ...) mnam deps defns M Σ)
   (cbody (Seq [At stx_new ph] body_rest ...) mnam deps defns M Σ)
   (where binding (resolve id_macro ph))
   (where [Tup Macro func] (lookup Σ binding))
   (where (Syntax stx_new) (eval (App func (Syntax (id_macro stx ...))) Σ))]
  ;; (require ....)
  ;;  Visit the required module, and add all of its bindings to
  ;;  the lexical context of the rest of the module. (Treat all
  ;;  definitions in the `require'd module as its exports.)
  [(cbody (Seq [At (id_req stx_in) ph] body_rest ...) mnam (Seq dep ...) defns M Σ)
   (cbody (Seq body_new ...) mnam (Seq dep ... dep_new) defns M Σ_new)
   (where (Bind ph base require) (resolve id_req ph))
   (where [Tup mnam_in ph_Δ] (parse-req stx_in ph))
   (where Σ_new (visit mnam_in ph_Δ M Σ))
   (where bindings_new (import mnam_in ph_Δ M))
   (where (Seq body_new ...) (Seq (add body_rest bindings_new) ...))
   (where dep_new [At mnam_in ph_Δ])]
  ;; (module ...)
  ;;  Declare a submodule. The submodule is compiled and added
  ;;  to the second of compiled modules, so that it is available
  ;;  for later forms in the module body. The lexical context
  ;;  so far accumulated for the submodule syntax object is
  ;;  discarded, so that the submodule starts afresh.
  [(cbody (Seq [At (id_mod id_sub id_init stx ...) ph] body_rest ...) mnam deps defns M Σ)
   (cbody (Seq body_rest ...) mnam deps defns M_new Σ)
   (where (Bind ph base module) (resolve id_mod ph))
   (where mnam_sub (strip id_sub))
   (where mnam_init (strip id_init))
   (where M_new
          (cmodule (module mnam_sub mnam_init (strip stx) ...) M))]
  ;; (module* ...)
  ;;  Declare a submodule. Finish the current module body, first,
  ;;  which adds the module to the set of compiled modules, and
  ;;  then compile the submodule. Note that we start with a fresh
  ;;  store for the submodule's compilation.
  [(cbody (Seq [At (id_mod id_sub id_init stx ...) ph] body_rest ...) mnam deps defns M Σ)
   (cmodule (module mnam_sub mnam_init (strip stx) ...) M_new)
   (where (Bind ph base module*) (resolve id_mod ph))
   (where mnam_sub (strip id_sub))
   (where mnam_init (strip id_init))
   (where M_new
          (cbody (Seq body_rest ...) mnam deps defns M Σ))]
  ;; (module* ... () ...)
  ;;  Declare a submodule that keeps its lexical context. We
  ;;  recur for submodule compilation using `cbody'
  ;;  since `cmodule' starts with a contextless source.
  [(cbody (Seq [At (id_mod id_sub () stx ...) ph] body_rest ...) mnam deps defns M Σ)
   (cbody (Seq [At (shift stx (p- 0 ph)) 0] ...) mnam_sub (Seq [At mnam (p- 0 ph)]) (Seq) M_new Σ_init)
   (where (Bind ph base module*) (resolve id_mod ph))
   (where mnam_sub (strip id_sub))
   (where M_new
          (cbody (Seq body_rest ...) mnam deps defns M Σ))
   (where Σ_init (visit mnam (p- 0 ph) M_new (Sto)))])

(module+ test
  ;; helper for tests:
  (define-metafunction L
    [(base+ binding ...)
     (Seq binding_r ... binding ...)
     (where (Seq binding_r ...) (import base 0 (Seq)))])

  (check-equal? (term (cbody (Seq) m (Seq) (Seq) (Seq) (Sto)))
                (term (Seq (Module m (Seq) (Seq)))))
  (check-equal? (term (cbody (Seq [At (wrap (define x 'ex)
                                            (base+)) 
                                      0])
                             m (Seq) (Seq)
                             (Seq) (Sto)))
                (term (Seq (Module m (Seq) 
                                   (Seq (Defn x 0 Value (Quote ex)))))))
  (check-equal? (term (cbody (Seq
                              [At (wrap
                                   (begin
                                     (define x 'ex)
                                     (define y 'ey))
                                   (base+))
                                  0])
                             m (Seq) (Seq)
                             (Seq) (Sto)))
                (term (Seq
                       (Module m (Seq)
                               (Seq
                                (Defn x 0 Value (Quote ex))
                                (Defn y 0 Value (Quote ey)))))))
  (check-equal? (term (cbody (Seq
                              [At (wrap (def)
                                        (Seq (Bind 0 mcro def)))
                                  0])
                             m (Seq) (Seq)
                             (Seq) (Sto
                                    (Cell (Bind 0 mcro def)
                                          Macro
                                          (Lambda (Seq (Var x))
                                                  (Syntax (wrap
                                                           (define x 'mx)
                                                           (base+))))))))
                (term (Seq (Module m (Seq) 
                                   (Seq (Defn x 0 Value (Quote mx)))))))
  (check-equal? (term (cbody (Seq
                              [At (wrap (module sub base
                                          (define s 'es))
                                        (base+))
                                  0])
                             m (Seq) (Seq)
                             (Seq) (Sto)))
                (term (Seq
                       (Module sub (Seq [At base 0])
                               (Seq (Defn s 0 Value (Quote es))))
                       (Module m (Seq) (Seq)))))
  (check-equal? (term (cbody (Seq
                              [At (wrap (module* sub base
                                          (define s 'es))
                                        (base+)) 
                                  0])
                             m (Seq) (Seq)
                             (Seq) (Sto)))
                (term (Seq
                       (Module m (Seq) (Seq))
                       (Module sub (Seq [At base 0])
                               (Seq (Defn s 0 Value (Quote es)))))))
  (check-equal? (term (cbody (Seq
                              [At (wrap (define x 'ex)
                                        (base+))
                                  0]
                              [At (wrap (module* sub ()
                                          (define s x))
                                        (base+))
                                  0])
                             m (Seq) (Seq)
                             (Seq) (Sto)))
                (term (Seq
                       (Module m (Seq) (Seq (Defn x 0 Value (Quote ex))))
                       (Module sub (Seq [At m 0]) 
                               (Seq (Defn s 0 Value (Ref (Bind 0 m x))))))))
  (check-equal? (term (cbody (Seq
                              [At (wrap (require base)
                                        (Seq (Bind 0 base require)))
                                  0]
                              [At (wrap (define x 'ex) (Seq))
                                  0])
                             m (Seq) (Seq)
                             (Seq) (Sto)))
                (term (Seq (Module m (Seq [At base 0])
                                   (Seq (Defn x 0 Value (Quote ex)))))))
  (check-equal? (term (cbody (Seq
                              [At (wrap (require base)
                                        (Seq (Bind 0 base require)))
                                  0]
                              [At (wrap (require (for-syntax base)) (Seq))
                                  0]
                              [At (wrap (define-syntax x 'ex) (Seq))
                                  0])
                             m (Seq) (Seq)
                             (Seq) (Sto)))
                (term (Seq
                       (Module m (Seq [At base 0] [At base 1])
                               (Seq (Defn x 0 Macro (Quote ex)))))))
  (check-equal? (term (cbody (Seq
                              [At (wrap (require base)
                                        (Seq (Bind 0 base require)))
                                  0]
                              [At (wrap (require (for-syntax base)) (Seq))
                                  0]
                              [At (wrap (begin-for-syntax
                                         (define c #'z))
                                        (Seq))
                                  0]
                              [At (wrap (define-syntax x c) (Seq))
                                  0])
                             m (Seq) (Seq)
                             (Seq) (Sto)))
                (term (Seq
                       (Module m (Seq [At base 0] [At base 1])
                               (Seq
                                (Defn c 1 Value (Syntax (add-to-stx
                                                         (wrap z
                                                               (import base 0 (Seq)))
                                                         (import base 1 (Seq)))))
                                (Defn x 0 Macro (Ref (Bind 1 m c)))))))))

;; cexpr
;;  Compiles an expression at the given phase, with
;;  the given list of enclosing `lambda' bindings,
;;  and with the given store (to be consulted for
;;  macro invocations).
(define-metafunction L
  cexpr : stx ph env Σ -> ast
  [(cexpr id ph env Σ)
   var
   (where var (env-lookup env (Var (strip id))))]
  [(cexpr id ph env Σ)
   (Ref binding)
   (where binding (resolve id ph))]
  [(cexpr (id_lambda (id ...) stx) ph (Seq nam_env ...) Σ)
   (Lambda (Seq (Var (strip id)) ...) (cexpr stx ph (Seq (Var (strip id)) ... nam_env ...) Σ))
   (where (Bind ph base lambda) (resolve id_lambda ph))]
  [(cexpr (id_quote stx) ph env Σ) 
   (Quote (strip stx))
   (where (Bind ph base quote) (resolve id_quote ph))]
  [(cexpr (id_qstx stx) ph env Σ)
   (Syntax stx)
   (where (Bind ph base syntax) (resolve id_qstx ph))]
  [(cexpr (id_macro stx ...) ph env Σ)
   (cexpr stx_new ph env Σ)
   (where binding (resolve id_macro ph))
   (where [Tup Macro func] (lookup Σ binding))
   (where (Syntax stx_new) (eval (App func (Syntax (id_macro stx ...))) Σ))]
  [(cexpr (stx ...) ph env Σ)
   (App (cexpr stx ph env Σ) ...)]
  [(cexpr id ph env Σ)
   Unbound])

(module+ test
  (check-equal? (term (cexpr (Stx x (Seq)) 0 (Seq (Var x)) (Sto)))
                (term (Var x)))
  (check-equal? (term (cexpr (Stx x (Seq (Bind 0 m x))) 0 (Seq) (Sto)))
                (term (Ref (Bind 0 m x))))
  (check-equal? (term (cexpr (wrap (lambda (x) (x y))
                                   (base+ (Bind 0 m x) (Bind 0 n y)))
                             0 (Seq) (Sto)))
                (term (Lambda (Seq (Var x)) (App (Var x) (Ref (Bind 0 n y))))))
  (check-equal? (term (cexpr (wrap (x 'x) (base+ (Bind 0 m x)))
                             0 (Seq) (Sto)))
                (term (App (Ref (Bind 0 m x)) (Quote x))))
  (check-equal? (term (cexpr (wrap (smbl) (Seq (Bind 0 m smbl)))
                             0 (Seq)
                             (Sto (Cell (Bind 0 m smbl)
                                        Macro
                                        (Lambda (Seq (Var x))
                                                (Syntax
                                                 (wrap 'sm
                                                       (base+))))))))
                (term (Quote sm))))

;; Evaluation ----------------------------------------

;; eval
;;  Evaluate an expression.
(define-metafunction L
  eval : ast Σ -> val
  [(eval val Σ) val]
  [(eval (Ref binding) Σ) 
   val
   (where [Tup Value val] (lookup Σ binding))]
  [(eval (App ast ast_arg ...) Σ)
   (eval (subst* ast_body [Seq var ...] [Seq val ...]) Σ)
   (where (Lambda (Seq var ...) ast_body)
          (eval ast Σ))
   (where (Seq val ...) (Seq (eval ast_arg Σ) ...))])

(module+ test
  (check-equal? (term (eval (Quote x) (Sto)))
                (term (Quote x)))
  (check-equal? (term (eval (Lambda (Seq (Var z)) (Var z)) (Sto)))
                (term (Lambda (Seq (Var z)) (Var z))))
  (check-equal? (term (eval (App (Lambda (Seq (Var z)) (Var z)) (Quote x)) (Sto)))
                (term (Quote x)))
  (check-equal? (term (eval (Ref (Bind 0 m x)) (Sto (Cell (Bind 0 m x) Value (Quote ok)))))
                (term (Quote ok))))

;; maybe-eval
;;  Evaluate only at phase other than 0; return a dummy
;;  value for phase 0.
(define-metafunction L
  maybe-eval : ph ast Σ -> val
  [(maybe-eval 0 ast Σ) Unbound]
  [(maybe-eval ph ast Σ) (eval ast Σ)])

;; ast-shift
;;  Shift the phase of an expression.
(define-metafunction L
  ast-shift : ast ph -> ast
  [(ast-shift var ph) var]
  [(ast-shift (Lambda (Seq var ...) ast) ph)
   (Lambda (Seq var ...) (ast-shift ast ph))]
  [(ast-shift (Quote nam) ph) (Quote nam)]
  [(ast-shift (App ast ...) ph) 
   (App (ast-shift ast ph) ...)]
  [(ast-shift (Ref (Bind ph_ref mnam dnam)) ph)
   (Ref (Bind (p+ ph_ref ph) mnam dnam))]
  [(ast-shift (Syntax stx) ph)
   (Syntax (shift stx ph))]
  [(ast-shift Unbound ph) Unbound])

;; subst*
;;  A helper for substitution.
(define-metafunction L
  subst* : ast (Seq var ...) (Seq val ...) -> ast
  [(subst* ast (Seq) (Seq)) ast]
  [(subst* ast (Seq var var_rest ...) (Seq val val_rest ...))
   (subst* (subst ast var val) (Seq var_rest ...) (Seq val_rest ...))])

(module+ test
  (check-equal? (term (subst* (App (Var x) (Var y) (Var z))
                              (Seq (Var x) (Var y))
                              (Seq (Quote q) (Quote n))))
                (term (App (Quote q) (Quote n) (Var z)))))

;; subst
;;  Substitution for beta reduction. We don't
;;  worry about capture, because we assume that
;;  no variables are free.
(define-metafunction L
  subst : ast var val -> ast
  [(subst var var val) val]
  [(subst var var_2 val) var]
  [(subst (Ref binding) var val) (Ref binding)]
  [(subst (Lambda (Seq var_arg ...) ast) var val)
   (Lambda (Seq var_arg ...) ast)
   (side-condition (member (term var) (term (var_arg ...))))]
  [(subst (Lambda (Seq var_arg ...) ast) var val)
   (Lambda (Seq var_arg ...) (subst ast var val))]
  [(subst (App ast ...) var val)
   (App (subst ast var val) ...)]
  [(subst (Quote nam_quoted) var val) (Quote nam_quoted)]
  [(subst (Syntax stx) var val) (Syntax stx)]
  [(subst Unbound var val) Unbound])

(module+ test
  (check-equal? (term (subst (App (Var x) (Var y) (Var z) (Ref (Bind 0 m x))) (Var x) (Quote q)))
                (term (App (Quote q) (Var y) (Var z) (Ref (Bind 0 m x)))))
  (check-equal? (term (subst (Lambda (Seq (Var x) (Var y) (Var z)) (Var x)) (Var x) (Quote q)))
                (term (Lambda (Seq (Var x) (Var y) (Var z)) (Var x))))
  (check-equal? (term (subst (Lambda (Seq (Var p) (Var y) (Var z)) (App (Var x) (Var w))) (Var x) (Quote q)))
                (term (Lambda (Seq (Var p) (Var y) (Var z)) (App (Quote q) (Var w))))))

;; Module Visit/Run ----------------------------------------

;; define-explore
;;  The `visit' and `run' metafunctions are similar in the
;;  way they expore the tree of imports. They differ in how
;;  each module body is handled. Both use the hack of mapping
;;  an `init' binding to 'yes to avoid multiple instantiations
;;  of the module at a given phase shift.
(define-syntax-rule/keep-srcloc (define-explore explore explore* explore-body)
  (begin
    (define-metafunction L
      explore : mnam ph M Σ -> Σ
      [(explore mnam ph_Δ M Σ)
       Σ
       (where Ready (ready-lookup Σ [At mnam ph_Δ]))]
      [(explore mnam ph_Δ M Σ)
       (explore-body (Seq defn ...) mnam ph_Δ Σ_deps)
       (where (Module mnam deps (Seq defn ...)) (mods-lookup M mnam))
       (where Σ_init (ready-extend Σ [At mnam ph_Δ] Ready))
       (where Σ_deps (explore* deps ph_Δ M Σ_init))])
    
    (define-metafunction L
      explore* : deps ph M Σ -> Σ
      [(explore* (Seq) ph_Δ M Σ) Σ]
      [(explore* (Seq [At mnam ph] dep ...) ph_Δ M Σ)
       (explore* (Seq dep ...) ph_Δ M Σ_new)
       (where Σ_new (explore mnam (p+ ph_Δ ph) M Σ))])))

;; visit
;;  Runs the compile-time part of a module and it dependencies,
;;  transitively.
(define-explore visit visit* visit-body)

;; visit-body
;;  Traverses the definitions of a single module at a given
;;  relative phase. Code at a negative phase (after shifting)
;;  is ignored, as is run-time code at phase 0.
(define-metafunction L
  visit-body : defns nam ph Σ -> Σ
  [(visit-body (Seq) mnam ph_Δ Σ) Σ]
  [(visit-body (Seq (Defn dnam ph_def Value ast) defn ...) mnam ph_Δ Σ)
   (visit-body (Seq defn ...) mnam ph_Δ Σ)
   (where 0 (p+ ph_Δ ph_def))]
  [(visit-body (Seq (Defn dnam ph_def kind ast) defn ...) mnam ph_Δ Σ)
   (visit-body (Seq defn ...) mnam ph_Δ Σ)
   (where -1 (pmax -1 (p+ ph_Δ ph_def)))]
  [(visit-body (Seq (Defn dnam ph_def kind ast) defn ...) mnam ph_Δ Σ)
   (visit-body (Seq defn ...) mnam ph_Δ Σ_new)
   (where ph (p+ ph_Δ ph_def))
   (where val (eval (shift ast ph_Δ) Σ))
   (where Σ_new
          (extend Σ (Bind ph mnam dnam) [Tup kind val]))])

;; run
;;  Runs the run-time part of a module and its dependencies,
;;  transitively.
(define-explore run run* run-body)

;; run-body
;;  Traverses the definitions of a single module at a given
;;  relative phase. Only run-time code at phase (after shifting)
;;  0 is run, and other code is ignored.
(define-metafunction L
  run-body : defns nam ph Σ -> Σ
  [(run-body (Seq) mnam ph_Δ Σ) Σ]
  [(run-body (Seq (Defn dnam ph_def Value ast) defn ...) mnam ph_Δ Σ)
   (run-body (Seq defn ...) mnam ph_Δ Σ_new)
   (where 0 (p+ ph_Δ ph_def))
   (where val (eval (shift ast ph_Δ) Σ))
   (where Σ_new
          (extend Σ (Bind 0 mnam dnam) [Tup Value val]))]
  [(run-body (Seq defn_skip defn ...) mnam ph_Δ Σ)
   (run-body (Seq defn ...) mnam ph_Δ Σ)])

(module+ test
  (check-equal? (term (visit m 0 (Seq (Module m (Seq) (Seq (Defn x 0 Value (Quote ex))))) (Sto)))
                (term (Sto (Cell [At m 0]))))
  (check-equal? (term (visit m 0 (Seq
                                  (Module m (Seq [At n 1]) (Seq (Defn x 0 Value (Quote ex))))
                                  (Module n (Seq) (Seq (Defn x 0 Value (Quote nex)))))
                             (Sto)))
                (term (Sto (Cell [At m 0])
                           (Cell [At n 1])
                           (Cell (Bind 1 n x) Value (Quote nex))))))

;; Syntax Objects ----------------------------------------

;; wrap
;;  Convert an S-expression to a syntax object with
;;  the given lexical context:
(define-metafunction L
  wrap : s-exp bindings -> stx
  [(wrap nam bindings) (Stx nam bindings)]
  [(wrap (s-exp ...) bindings) ((wrap s-exp bindings) ...)])

;; strip
;;  Convert a syntax object to an S-expression, dropping
;;  a lexical context.
(define-metafunction L
  strip : stx -> s-exp
  [(strip (Stx nam bindings)) nam]
  [(strip (stx ...)) ((strip stx) ...)])

;; add-to-stx
;;  Extend a syntax object's lexical context with
;;  next bindings.
(define-metafunction L
  add-to-stx : stx bindings -> stx
  [(add-to-stx (Stx nam (Seq binding ...)) (Seq binding_new ...))
   (Stx nam (Seq binding ... binding_new ...))]
  [(add-to-stx (stx ...) bindings) ((add-to-stx stx bindings) ...)])

;; add
;;  Uses `add-to-stx' on a body's syntax object.
(define-metafunction L
  add : body bindings -> body
  [(add [At stx ph] bindings)
   [At (add-to-stx stx bindings) ph]])

;; resolve
;;  Consults an identifier's lexical context to determine
;;  the module definition that it refers to.
(define-metafunction L
  resolve : id ph -> maybe-binding
  [(resolve (Stx dnam (Seq binding_pre ... binding binding_post ...)) ph)
   binding
   (where (Bind ph mnam dnam) binding)]
  [(resolve id ph) #f])

(module+ test
  (check-equal? (term (resolve (Stx x (Seq (Bind 0 m x) (Bind 1 n x))) 0))
                (term (Bind 0 m x)))
  (check-equal? (term (resolve (Stx x (Seq (Bind 0 m x) (Bind 1 n x))) 1))
                (term (Bind 1 n x))))

;; shift
;;  Shift the phase of a syntax object.
;;  Also allow an ast, effectively overloading `shift'.
(define-metafunction L
  shift : stx-or-ast ph -> stx-or-ast
  [(shift (Stx nam (Seq (Bind ph mnam dnam) ...)) ph_shift)
   (Stx nam (Seq (Bind (p+ ph ph_shift) mnam dnam) ...))]
  [(shift (stx ...) ph_shift)
   ((shift stx ph_shift) ...)]
  [(shift ast ph_shift)
   (ast-shift ast ph_shift)])

;; Store ----------------------------------------

;; lookup
;;  Look up a definition (key on phase, module, and
;;  defined name) in the store. Fails with #f so that
;;  `lookup' can be used as a side condition.
(define-metafunction L
  lookup : Σ binding -> lookup-result
  [(lookup (Sto) binding) #f]
  [(lookup (Sto (Cell binding kind val) cell ...) binding)
   [Tup kind val]]
  [(lookup (Sto cell_0 cell ...) binding)
   (lookup (Sto cell ...) binding)])

;; extend
;;  Extend the store with a definition.
(define-metafunction L
  extend : Σ binding [Tup kind val] -> Σ
  [(extend (Sto cell ...) binding [Tup kind val])
   (Sto cell ... (Cell binding kind val))])

;; ready-lookup
;;  Look up whether a module has been instantiated
;;  at a given phase.
(define-metafunction L
  ready-lookup : Σ ready-binding -> ready-lookup-result
  [(ready-lookup (Sto) ready-binding) #f]
  [(ready-lookup (Sto (Cell ready-binding) cell ...) ready-binding)
   Ready]
  [(ready-lookup (Sto cell_0 cell ...) ready-binding)
   (ready-lookup (Sto cell ...) ready-binding)])

;; ready-extend
;;  Extend the store with a record that a module has
;;  been instantiated at a particular phase.
(define-metafunction L
  ready-extend : Σ ready-binding Ready -> Σ
  [(ready-extend (Sto cell ...) ready-binding Ready)
   (Sto cell ... (Cell ready-binding))])

(module+ test
  (check-equal? (term (visit-body (Seq (Defn x 0 Value (Quote xes))) ex 0 (Sto)))
                (term (Sto)))
  (check-equal? (term (visit-body (Seq (Defn x 1 Value (Quote xes))) ex 0 (Sto)))
                (term (Sto (Cell (Bind 1 ex x) Value (Quote xes)))))
  (check-equal? (term (visit-body (Seq
                                        (Defn x 1 Value (Quote xes))
                                        (Defn y 0 Macro (Quote yes)))
                                  ex 0 (Sto (Cell (Bind 2 other z) Value (Quote zee)))))
                (term (Sto (Cell (Bind 2 other z) Value (Quote zee))
                           (Cell (Bind 1 ex x) Value (Quote xes))
                           (Cell (Bind 0 ex y) Macro (Quote yes))))))

;; Other helpers ----------------------------------------

;; mods-lookup
;;  Find a declared module, or synthesizes the
;;  kernel module named `base'.
(define-metafunction L
  mods-lookup : M nam -> mod
  [(mods-lookup M base)
   (Module base (Seq)
           (Seq
            (Defn lambda 0 Macro (Quote primitive))
            (Defn quote 0 Macro (Quote primitive))
            (Defn syntax 0 Macro (Quote primitive))
            (Defn define 0 Macro (Quote primitive))
            (Defn define-syntax 0 Macro (Quote primitive))
            (Defn begin-for-syntax 0 Macro (Quote primitive))
            (Defn begin 0 Macro (Quote primitive))
            (Defn require 0 Macro (Quote primitive))
            (Defn module 0 Macro (Quote primitive))
            (Defn module* 0 Macro (Quote primitive))))]
  [(mods-lookup (Seq mod_1 ... 
                     (name mod (Module nam deps (Seq defn ...)))
                     mod_2 ...)
                nam)
   mod])

;; p+, p-
;;  Phase arithmetic.
(define-metafunction L
  p+ : ph ph -> ph
  [(p+ ph_n ph_m) ,(+ (term ph_n) (term ph_m))])
(define-metafunction L
  p- : ph ph -> ph
  [(p- ph_n ph_m) ,(- (term ph_n) (term ph_m))])
(define-metafunction L
  pmax : ph ph -> ph
  [(pmax ph_n ph_m) ,(max (term ph_n) (term ph_m))])  

;; parse-req
;;  Extracts the relative phase and module name for
;;  a `require' argument.
(define-metafunction L
  parse-req : stx ph -> [Tup nam ph]
  [(parse-req id_in ph)
   [Tup (strip id_in) ph]]
  [(parse-req (id_fs stx_in) ph)
   (parse-req stx_in (p+ ph 1))
   (where for-syntax (strip id_fs))]
  [(parse-req (id_ft stx_in) ph)
   (parse-req stx_in (p- ph 1))
   (where for-template (strip id_ft))])

(module+ test
  (check-equal? (term (parse-req (wrap base (Seq)) 0))
                (term [Tup base 0]))
  (check-equal? (term (parse-req (wrap (for-syntax other) (Seq)) 1))
                (term [Tup other 2]))
  (check-equal? (term (parse-req (wrap (for-template (for-syntax other)) (Seq)) 3))
                (term [Tup other 3])))


;; import
;;  Locates a compiled module and generates lexical context
;;  to import all of the module's definitions.
(define-metafunction L
  import : mnam ph_Δ M -> bindings
  [(import mnam ph_Δ M)
   (Seq (Bind (p+ ph_Δ ph_def) mnam dnam) ...)
   (where (Module mnam (Seq dep ...) (Seq defn ...))
          (mods-lookup M mnam))
   (where (Seq (Defn dnam ph_def kind ast) ...)
          (Seq defn ...))])

;; env-lookup
;;  Check whether an identifier is in the environment.
(define-metafunction L
  env-lookup : env var -> any
  [(env-lookup (Seq) var) #f]
  [(env-lookup (Seq var var_rest ...) var) var]
  [(env-lookup (Seq var_2 var_rest ...) var) (env-lookup (Seq var_rest ...) var)])

(module+ test
  (check-equal? (term (env-lookup (Seq) (Var x))) (term #f))
  (check-equal? (term (env-lookup (Seq (Var a) (Var b) (Var c)) (Var x))) (term #f))
  (check-equal? (term (env-lookup (Seq (Var a) (Var b) (Var c)) (Var b))) (term (Var b))))

;; run/main-result
;;  Compiles a sect of modules, runs the `main' module,
;;  and extracts the value of the module's `result'
;;  definition from the store.
(define-metafunction L
  [(run/main-result s-exp ...)
   val
   (where M (cmodule* (s-exp ...) (Seq)))
   (where Σ (run main 0 M (Sto)))
   (where [Tup Value val] (lookup Σ (Bind 0 main result)))])

;; cmodule*
;;  Folds `cmodule' over a sequence of modules.
(define-metafunction L
  cmodule* : (s-exp ...) M -> M
  [(cmodule* () M) M]
  [(cmodule* (s-exp s-exp_rest ...) M)
   (cmodule* (s-exp_rest ...) (cmodule s-exp M))])

(define-metafunction L
  mods-extend : M mnam mod -> M
  [(mods-extend (Seq mod ...) mnam mod_new)
   (Seq mod ... mod_new)])

