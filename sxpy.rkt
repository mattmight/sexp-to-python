#lang racket


; indent : int parameter
; indent is a dynamic variable to track the output indentation level.
(define indent (make-parameter 0))

; (with-indent <body>) increases the output indentation level.
(define-syntax with-indent
  (syntax-rules ()
    [(_ body ...)
     (parameterize ([indent (+ 2 (indent))])
       body ...)]))

; indent-spaces : -> string
; indent-spaces returns a string containing spaces
; equal to the current level of indentation.
(define (indent-spaces) 
  (build-string (indent) (λ (i) #\space)))


;; Modules
(define (module->string module)
  (match module
    [`(Module . ,stmts)
     (stmts->string stmts)]))
  

;; Operators
(define (op->string op)
    (match op
    ['And    " and "]
    ['Or     " or "]
    
    ['Invert  "~"]
    ['Not     "!"]
    ['UAdd    "+"]
    ['USub    "-"]

    ; Add 
    ['Add    " + "]

    ; Sub 
    ['Sub    " - "]

    ; Mult 
    ['Mult   " * "]

    ; Div 
    ['Div    " / "]

    ; Mod 
    ['Mod    " % "]
    
    ; Pow 
    ['Pow    " ** "]

    ; LShift 
    ['LShift  " << "]
                     
    ; RShift 
    ['RShift  " >> "]

    ; BitOr 
    ['BitOr   " | "]

    ; BitXor 
    ['BitXor  " ^ "]

    ; BitAnd 
    ['BitAnd   " & "]
    
    ; FloorDiv
    ['FloorDiv " // "]
    
    ; Eq 
    ['Eq " == "]
    
    ; NotEq 
    ['NotEq " != "]
    
    ; Lt 
    ['Lt " < "]
    
    ; LtE 
    ['LtE " <= "]
    
    ; Gt 
    ['Gt " > "]
     
    ; GtE
    ['GtE " >= "]
    
    ; Is 
    ['Is " is " ]
    
    ; IsNot 
    ['IsNot " is not "]
    
    ; In 
    ['In " in "]
    
    ; NotIn
    ['NotIn " not in "]))


(define (augop->string op)
  (match op

    ; Add 
    ['Add    " += "]

    ; Sub 
    ['Sub    " -= "]

    ; Mult 
    ['Mult   " *= "]

    ; Div 
    ['Div    " /= "]

    ; Mod 
    ['Mod    " %= "]
    
    ; Pow 
    ['Pow    " **= "]

    ; LShift 
    ['LShift  " <<= "]
                     
    ; RShift 
    ['RShift  " >>= "]

    ; BitOr 
    ['BitOr   " |= "]

    ; BitXor 
    ['BitXor  " ^= "]

    ; BitAnd 
    ['BitAnd   " &= "]
    
    ; FloorDiv
    ['FloorDiv " //= "]))



;; Expressions
(define (expr->string expr)
  (match expr
    
    ; (BoolOp <boolop> <expr>*)
    [`(BoolOp ,op . ,exprs)
      (string-join (map expr->string exprs) (op->string op))]

    ; (BinOp <expr> <operator> <expr>)
    [`(BinOp ,lhs ,op ,rhs)
      (string-append 
         "(" (expr->string lhs) ")"
          (op->string op)
         "(" (expr->string rhs) ")"
        )]

    ; (UnaryOp <unaryop> <expr>)
    [`(UnaryOp ,op ,expr)
     (string-append (op->string op) "(" (expr->string expr) ")")]

    ; (Lambda <arguments> <expr>)
    [`(Lambda ,parameters ,expr)
     (string-append "lambda " 
                    (parameters->string parameters)
                    ": (" (expr->string expr) ")")]

    ; (IfExp <expr> <expr> <expr>)
    [`(If ,cond ,ontrue ,onfalse)
     (string-append "(" 
                    (expr->string ontrue)
                    ") if ("
                    (expr->string cond)
                    ") else ("
                    (expr->string onfalse)
                    ")")]

    ; (Dict (keys <expr>*) (values <expr>*))
    [`(Dict (keys . ,keys) (values . ,values))
     (string-append
      "{"
      (string-join 
       (map
        (λ (k v) (string-append (expr->string k) ":" (expr->string v)))
        keys values)
       ",")
      "}")]
    
    ; (Set <expr>*)
    [`(Set . ,exprs)
      (string-append "{" (string-join (map expr->string exprs) ",") "}")]

    ; (ListComp <expr> <comprehension>*)
    [`(ListComp ,expr ,comp)
     (string-append "[" (expr->string expr) (comprehension->string comp) "]")]
    
    ; (SetComp <expr> <comprehension>*)
    [`(SetComp ,expr ,comp)
     (string-append "{" (expr->string expr) (comprehension->string comp) "}")]
    
    ; (DictComp <expr> <expr> <comprehension>*)
    [`(DictComp ,key ,value ,comp)
     (string-append "{" (expr->string key) " : " (expr->string value) (comprehension->string comp) "}")]
    
    ; (GeneratorExp <expr> <comprehension>*)
    [`(GeneratorExp ,expr ,comp)
     (string-append "(" (expr->string expr) (comprehension->string comp) ")")]
    

    ; (Yield)  | (Yield <expr>)
    [`(Yield)
     "yield"]
    
    [`(Yield ,expr)
     (string-append 
      "yield (" 
      (expr->string expr)
      ")")]
      
    ; (YieldFrom <expr>)
    [`(YieldFrom ,expr)
     (string-append 
      "(yield from " 
      (expr->string expr)
      ")")]


    ; (Compare (left        <expr>) 
    ;          (ops         <cmpop>*)
    ;          (comparators <expr>*))
    [`(Compare (left ,expr) (ops . ,ops) (comparators . ,exprs))
     (string-append 
      (expr->string expr)
      (string-join (map (λ (o e) (string-append
                                  (op->string o)
                                  (expr->string e))) ops exprs) " "))]
      

    ; (Call (func <expr>)
    ;       (args <expr>*)
    ;       (keywords <keyword>*)
    ;       (starargs <expr?>)
    ;       (kwargs <expr?>))
    [`(Call (func ,func)
            (args . ,args)
            (keywords . ,keywords)
            (starargs ,starargs)
            (kwargs ,kwargs))
     ;=>
     (string-append
      (match func 
        [`(Name ,name) (symbol->string name)]
        [else (string-append "(" 
                             (expr->string func)
                             ")")])
      "("
      (arguments->string args keywords starargs kwargs)
      ")"
      )]

    ; (Num <number>)
    ; TODO/BUG: Handle complex numbers:
    [`(Num ,num)           (number->string num)]
    
    ; (Str <string>)
    ; TODO/BUG: Handle this correctly:
    [`(Str ,str)
     (format "~s" str)]
    
    ; (Bytes <byte-string>)
    ; TODO/BUG: 0-pad hex values less than 16
    [`(Bytes ,bytes)
     (string-append 
      "b'"
      (apply string-append (map (λ (b)
                                (string-append "\\x" (number->string b 16)))
                                (bytes->list bytes)))
      "'")]

    ; (NameConstant <name-constant>)
    [`(NameConstant True)  "True"]
    
    ; (Ellipsis)
    ['(Ellipsis)     "..."]

    ; (Attribute <expr> <identifier>)
    [`(Attribute (Name ,name) ,id)
     (string-append (symbol->string name) "." (symbol->string id))]
    
    
    [`(Attribute ,expr ,id)
     (string-append "(" (expr->string expr) ")" "." (symbol->string id))]
    
    ; (Subscript <expr> <slice>)
    [`(Subscript ,expr ,slice)
     (string-append "(" (expr->string expr) ")" 
                    "[" (slice->string slice) "]")]
    
    ; (Starred <expr>)
    [`(Starred ,expr)
     (string-append "(*" (expr->string expr) ")")]
    
    ; (Name <identifier>)
    [`(Name ,var)          (symbol->string var)]

    ; (List <expr>*)
    [`(List . ,exprs)
     (string-append "[" (string-join (map expr->string exprs) ",") "]")]
        
    ; (Tuple <expr>*)
    [`(Tuple . ,exprs)
     (string-append "(" (string-join (map expr->string exprs) ",") "," ")")]))
    



;; Slices
(define (slice->string slice)
  
  (define (expr?->string expr)
    (if expr
        (expr->string expr)
        ""))
  
  (match slice
    [`(Index ,expr)  (expr->string expr)]
    
    [`(Slice ,lo ,hi ,skip) 
     (string-append (expr?->string lo)
                    ":" (expr?->string hi)
                    ":" (expr?->string skip))]
    
    [else (error (format "unknown slice: ~s" slice))]))
    

;; Arguments
(define (arguments->string args keywords starargs kwargs)

  (define (keyword->string keyword)
    (match keyword
      [`(,name ,value)
       (format "~a = ~a" name (expr->string value))]))
  
  (define printed-args 
    (append (map expr->string args)
            (map keyword->string keywords)))
  
  (when starargs
    (set! starargs 
          (append printed-args 
                  (list (string-append "*" "(" (expr->string starargs) ")")))))
  
  (when kwargs
    (set! kwargs 
          (append printed-args 
                  (list (string-append "**" "(" (expr->string kwargs) ")")))))
    
  (string-join printed-args ","))


;; Decorators
(define (decorators->string decorators)
  (if (null? decorators) 
      ""
      "TODO: decorators"))


;; Comprehensions
(define (comprehension->string comp)
  (match comp
    [`(for ,target in ,iter if)
     (string-append " for " (expr->string target) " in " (expr->string iter))]

    [`(for ,target in ,iter if . ,conds)
     (string-append " for " (expr->string target)
                    " in " (expr->string iter)
                    " if " (string-join (map expr->string conds) " if "))]))
                    

    
;; Parameters
(define (parameters->string parameters)
  (match parameters
    ; (Arguments
    ;     (args <arg>*)
    ;     (arg-types <expr?>*)
    ;     (vararg <arg?> <expr>?) 
    ;     (kwonlyargs <arg>*)
    ;     (kwonlyarg-types <expr?>*)
    ;     (kwdefaults <expr>*)
    ;     (kwarg <arg?> <expr>?) 
    ;     (defaults <expr?>*))
    [`(Arguments
       (args . ,args)
       (arg-types . ,arg-types)
       (vararg ,vararg) ; TODO/BUG: Handle possible type
       (kwonlyargs . ,kwonlyargs)
       (kwonlyarg-types . ,kwonlyarg-types)
       (kw_defaults . ,kw_defaults)
       (kwarg . ,kwarg) ; TODO/BUG: Handle possible type
       (defaults . ,defaults))
     ; =>
     (define printed-args '())
     
     (define (add-arg! arg)
       (set! printed-args (append printed-args (list arg))))
     
     (define render-arg
       (λ (a t d) 
         
         (define p 
           (if d (format "~a = ~a"
                         (symbol->string a) 
                         (expr->string d)) 
               (symbol->string a)))
         
         (if t
             (format "~a : ~a" 
                     p
                     (expr->string t))
             p)))
       
     (set! printed-args 
           (map render-arg
                args
                arg-types
                defaults))
     
     (when (and (not vararg) (not (null? kwonlyargs)))
       (add-arg! "*"))
     
     (when vararg
       (add-arg! (string-append "*" (symbol->string vararg))))
     
     (set! printed-args 
           (append printed-args (map render-arg
                                     kwonlyargs
                                     kwonlyarg-types
                                     kw_defaults)))
                             
     (string-join printed-args ",")]))
       

;; Exception handlers:
(define (handler->string handler)

  (match handler
    [`(except #f #f . ,body)
     (string-append 
      (indent-spaces)
      "except:\n"
      (with-indent (stmts->string body)))]
    
    [`(except ,exn #f . ,body)
     (string-append 
      (indent-spaces)
      "except " (expr->string exn) ":\n"
      (with-indent (stmts->string body)))]
    
    [`(except ,exn ,name . ,body)
     (string-append 
      (indent-spaces)
      "except "
      (expr->string exn)
      " as "
      (symbol->string name)
      ":\n"
      (with-indent (stmts->string body)))]
    
    [else
     (error (format "ill-structured handler ~a" handler))]))
         

(define (handlers->string handlers)
  (apply string-append (map handler->string handlers)))
       

;; Aliases
(define (alias->string alias)
  (match alias
    [`(,module-name #f)
     (symbol->string module-name)]
    
    [`(,module-name ,name)
     (string-append 
      (symbol->string module-name)
      " as "
      (symbol->string name))]))


;; Statements:
(define (stmt->string stmt)
  (match stmt
    
    ; (FunctionDef
    ;   (name <identifier>)
    ;   (args <arguments>)
    ;   (body <stmt>*)
    ;   (decorator_list <expr>*)
    ;   (returns <expr?>))
    [`(FunctionDef (name ,id) (args ,parameters) (body . ,body) 
                   (decorator_list . ,decorators)
                   (returns . ,returns))
     (string-append
      (decorators->string decorators)
      "def " (symbol->string id) "(" (parameters->string parameters) ")" 
      (if (not returns) (string-append " -> " (expr->string returns)) "")
      ":\n"
      (with-indent
       (stmts->string body)))]
    
    
    ; (ClassDef
    ;   (name <identifier>)
    ;   (bases <expr>*)
    ;   (keywords <keyword>*)
    ;   (starargs <expr?>)
    ;   (kwargs <expr?>)
    ;   (body <stmt>*)
    ;   (decorator_list <expr>*))
    [`(ClassDef 
       (name ,name)
       (bases . ,bases)
       (keywords . ,keywords)
       (starargs ,starargs)
       (kwargs ,kwargs)
       (body . ,body)
       (decorator_list . ,decorator_list))
     (string-append 
      (decorators->string decorator_list)
      "class " (symbol->string name) "(" (arguments->string bases keywords starargs kwargs) "):\n"
      (indent-spaces) 
      (with-indent (stmts->string body)))]
    
    
    
    ; (Return <expr?>)
    [`(Return)          "return"]
    [`(Return ,expr)    (string-append "return " (expr->string expr))]
    
    ; (Delete <expr>*)
    [`(Delete . ,exprs) (string-append "delete " (string-join (map expr->string exprs) ", "))]
    
    ; (Assign (targets <expr>*) (value <expr>))
    [`(Assign (targets . ,targets) (value ,value))
     (define lhses (map expr->string targets))
     (string-append (string-join lhses " = " )
                    " = "
                    (expr->string value))]
    
    
    ; (AugAssign <expr> <operator> <expr>)
    [`(AugAssign ,lhs ,op ,rhs)
     (string-append (expr->string lhs) (augop->string op) (expr->string rhs))]
    
    ; (For (target <expr>) (iter <expr>) (body <stmt>*) (orelse <stmt>*))
    [`(For (target , target) (iter ,iter) (body . ,body) (orelse))
     (string-append 
      "for " (expr->string target) " in " (expr->string iter) ":\n"
      (with-indent (stmts->string body)))]
    
    [`(For (target , target) (iter ,iter) (body . ,body) (orelse . ,orelse))
     (string-append 
      "for " (expr->string target) " in " (expr->string iter) ":\n"
      (with-indent (stmts->string body))
      (indent-spaces) "else:\n"
      (with-indent
       (stmts->string orelse)))]
    
    
    ; (While (test <expr>) (body <stmt>*) (orelse <stmt>*))
    [`(While (test ,test) (body . ,body) (orelse))
     (string-append "while " (expr->string test) ":\n" 
                    (with-indent
                     (stmts->string body)))]
    
    [`(While (test ,test) (body . ,body) (orelse . ,orelse))
     (string-append "while " (expr->string test) ":\n" 
                    (with-indent
                     (stmts->string body))
                    (indent-spaces) "else:\n"
                    (with-indent
                     (stmts->string orelse)))]
    
    
    
    ; (If (test <expr>) (body <stmt>*) (orelse <stmt>*))
    [`(If (test ,test) (body . ,body) (orelse))
     (string-append "if " (expr->string test) ":\n" 
                    (with-indent
                     (stmts->string body)))]
    
    [`(If (test ,test) (body . ,body) (orelse . ,orelse))
     (string-append "if " (expr->string test) ":\n" 
                    (with-indent
                     (stmts->string body))
                    (indent-spaces) "else:\n"
                    (with-indent
                     (stmts->string orelse)))]
    
    
    ; (With [<withitem>*] <stmt>*)
    [`(With ,withitems . ,body)
     
     (define (withitem->string withitem)
       (match withitem
         [`(,ctxt #f)
          (expr->string ctxt)]
         
         [`(,ctxt ,alias)
          (string-append (expr->string ctxt) " as " (expr->string alias))]))
     
     (string-append "with " (string-join (map withitem->string withitems) ", ") ":\n"
                    (with-indent (stmts->string body)))]
    
    ; (Raise)
    [`(Raise)
     "raise"]
    
    ; (Raise <expr>) 
    [`(Raise ,expr)
     (string-append "raise " (expr->string expr))]
    
    ; (Raise <expr> <expr>)
    [`(Raise ,expr ,from)
     (string-append "raise " (expr->string expr) " from " (expr->string from))]
    
    ; (Try (body <stmt>*)
    ;      (handlers <excepthandler>*)
    ;      (orelse <stmt>*)
    ;      (finalbody <stmt>*))
    [`(Try (body . ,body)
           (handlers . ,handlers)
           (orelse . ,orelse)
           (finalbody . ,finalbody))
     (string-append
      "try:\n"
      (with-indent (stmts->string body))
      (handlers->string handlers)
      (if (not (null? orelse)) 
          (string-append (indent-spaces) "else:\n"
                         (with-indent (stmts->string orelse)))
          "")
      (if (not (null? finalbody))
          (string-append (indent-spaces) "finally:\n" 
                         (with-indent (stmts->string finalbody)))
          ""))]

    
    ; (Assert <expr>)
    [`(Assert ,expr)
     (string-append "assert " (expr->string expr))]
    
    ; (Assert <expr> <expr>)
    [`(Assert ,test ,msg)
     (string-append "assert " (expr->string test) ", " (expr->string msg))]
    
    ;(Import <alias>*)
    [`(Import . ,aliases)
     (string-append
      "import "
      (string-join (map alias->string aliases) ", "))]
    
    ; (ImportFrom (module <identifier?>)
    ;             (names <alias>*)
    ;             (level <int?>))
    [`(ImportFrom (module ,id)
                  (names . ,aliases)
                  (level ,level))
     (error "TODO/BUG: ImportFrom not yet handled.")]
       
    
    ; (Global <identifier>+)
    [`(Global . ,ids)
     (string-append "global " (string-join (map symbol->string ids) ", "))]
    
    ; (Nonlocal <identifier>+)
    [`(Nonlocal . ,ids)
     (string-append "nonlocal " (string-join (map symbol->string ids) ", "))]
    
    ;(Expr <expr>)
    [`(Expr, expr)
     (expr->string expr)]
    
    ;(Pass) 
    ['(Pass)      "pass"]

    ;(Break)
    ['(Break)     "break"]
    
    ;(Continue)
    ['(Continue)  "continue"]
    
    ))
    
    

(define (stmts->string stmts)
  (string-join (map stmt->string stmts) 
               (string-append "\n" (indent-spaces)) 
               #:before-first (indent-spaces)
               #:after-last "\n"))

#;(display (module->string
          '(Module (If (test (NameConstant True)) 
                       (body (If (test (NameConstant True))
                                 (body (Pass) (Pass))
                                 (orelse (Pass) (Pass) (Pass))))
                       (orelse))
                   
                   (Delete (Name x)))))


#;(display (module->string
          '(Module
 (FunctionDef
  (name f)
  (args
   (Arguments
    (args x y z)
    (arg-types #f #f #f)
    (vararg #f)
    (kwonlyargs)
    (kwonlyarg-types)
    (kw_defaults)
    (kwarg #f)
    (defaults)))
  (body (Pass))
  (decorator_list)
  (returns #f)))))
    

(display (module->string (read)))

    
