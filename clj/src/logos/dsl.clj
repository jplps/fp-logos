(ns logos.dsl
  "Composable expression interpreter using protocols and records.
   Solves the expression problem à la Swierstra (2008).
   New operations and new expression types can be added
   without modifying existing code.")

(defprotocol Eval
  "Evaluation protocol for expression types."
  (evaluate [expr env]
    "Evaluates an expression in the given environment.
     expr : expression record
     env : map of variable bindings
     returns : numeric value
     O(n) in expression tree size"))

(defprotocol PrettyPrint
  "Pretty-printing protocol for expression types."
  (pp [expr]
    "Returns a string representation of the expression.
     expr : expression record
     returns : string
     O(n) in expression tree size"))


(defrecord Lit [value]
  Eval
  (evaluate [_ _env] value)
  PrettyPrint
  (pp [_] (str value)))

(defrecord Var [name]
  Eval
  (evaluate [_ env]
    (if-let [v (get env name)]
      v
      (throw (ex-info "unbound variable" {:name name}))))
  PrettyPrint
  (pp [_] (if (keyword? name) (clojure.core/name name) (str name))))

(defrecord Add [left right]
  Eval
  (evaluate [_ env]
    (+ (evaluate left env) (evaluate right env)))
  PrettyPrint
  (pp [_] (str "(" (pp left) " + " (pp right) ")")))

(defrecord Mul [left right]
  Eval
  (evaluate [_ env]
    (* (evaluate left env) (evaluate right env)))
  PrettyPrint
  (pp [_] (str "(" (pp left) " * " (pp right) ")")))

(defrecord Neg [operand]
  Eval
  (evaluate [_ env]
    (- (evaluate operand env)))
  PrettyPrint
  (pp [_] (str "(- " (pp operand) ")")))

(defrecord Let [var-name binding body]
  Eval
  (evaluate [_ env]
    (let [val (evaluate binding env)]
      (evaluate body (assoc env var-name val))))
  PrettyPrint
  (pp [_]
    (let [vn (if (keyword? var-name) (clojure.core/name var-name) (str var-name))]
      (str "(let [" vn " " (pp binding) "] " (pp body) ")"))))

(defrecord IfZero [test then else-branch]
  Eval
  (evaluate [_ env]
    (if (zero? (evaluate test env))
      (evaluate then env)
      (evaluate else-branch env)))
  PrettyPrint
  (pp [_]
    (str "(if-zero " (pp test) " " (pp then)
         " " (pp else-branch) ")")))


(defn lit
  "Constructs a literal expression.
   v : numeric value
   returns : Lit record
   O(1)"
  [v]
  (->Lit v))

(defn vr
  "Constructs a variable reference expression.
   name : keyword or symbol
   returns : Var record
   O(1)"
  [name]
  (->Var name))

(defn add
  "Constructs an addition expression.
   l, r : expressions
   returns : Add record
   O(1)"
  [l r]
  (->Add l r))

(defn mul
  "Constructs a multiplication expression.
   l, r : expressions
   returns : Mul record
   O(1)"
  [l r]
  (->Mul l r))

(defn neg
  "Constructs a negation expression.
   e : expression
   returns : Neg record
   O(1)"
  [e]
  (->Neg e))

(defn let-expr
  "Constructs a let-binding expression.
   var-name : keyword
   binding : expression
   body : expression
   returns : Let record
   O(1)"
  [var-name binding body]
  (->Let var-name binding body))

(defn if-zero
  "Constructs a conditional-on-zero expression.
   test : expression
   then : expression
   else-branch : expression
   returns : IfZero record
   O(1)"
  [test then else-branch]
  (->IfZero test then else-branch))
