(ns crispy-docs.core
    (:require [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [clojure.string :as string]))
;; -------------------------
;; App specific functions and definitions

(def code-fmt [:code>span {:style {:background-color "mediumaquamarine"}}])

(def code-block-fmt [:pre>code {:style {:background-color "mediumaquamarine"
                                        :line-height "1em"
                                        :display "block"
                                        :border "1px solid black"
                                        :padding "20px"}}])

(defn code-tags [code-line]
  (conj code-fmt code-line))

(defn list-sections [doc-list]
  (vec (cons
         (vec (cons :div
                    (mapv
                      (fn [section]
                        [:div>a {:href (str "#" (:name section))} (:name section)])
                      doc-list)))
         (mapv
           (fn [section]
             (let [{header :name body :body} section]
               [:div [:a {:id header}]
                [:h3  header]
                (vec (cons :p>:content  body))]))
           doc-list))))

(defn lister [items]
  (vec (cons :ul (mapv (fn [item]
                         (vec (cons :li item)))
                       items))))

(defn expr-result [expr result]
  ["Example: "
   (code-tags expr)
   " => " (code-tags result)])

(defn symbols
  ([smbl] ["Symbosl: " (code-tags smbl)])
  ([smbl & smbls] (vec ( cons "Symbols:"
                        ( reduce #(concat [%1] ", " [%2])
                           (map code-tags (cons smbl smbls)))))))

(defn desc [& lines]
  (vec (cons "Purpose: " lines)))

(defn arity
  ( [] (arity "no"))
  ( [args] [(str "Arity: takes " args " operands.")]))

(defn break-lines [lines]
  (reduce #(conj (conj %1 "\n" ) %2) [] lines))

(defn code-block [lines]
  [(vec (concat code-block-fmt (break-lines lines)))])

(def about-sections [{:name "About"
                      :body ["Crispy is a Lisp designed to use as little built-in syntax, "
                             "and as few special forms as possible."
                             [:br] [:br]
                             " It is written in C, and avoids looping constructs in favor of recursion. "
                             "Though it does have one loop--for the REPL!"
                             [:br] [:br]
                             "It currently uses Clangs TCO build option"
                             " however many of the recursive functions are currently not TCO compatible."
                             [:br] [:br]
                             "The codebase is located " [:a {:href "https://github.com/thea-leake/crisp"} "here"]
                             "."]}

                     {:name "Syntax"
                      :body  [ "Crispy's Syntax consists of lists of expressions."
                              [:br]
                              "These expressions are represented by"
                              (lister [[" Lists: " (code-tags "(+ 1 3 )")]
                                       ["Values: " (code-tags "18 \"st\" 2.4 true nil")]
                                       ["Symbols (including builtins): " (code-tags "a-zA-Z:!_%=?-+*")]
                                       ["Deferment: " (code-tags "'")]])]}


                     {:name "Lists"
                      :body [(lister [["Lists are defined by an opening and closing parenthesis."]
                                      ["Values in the list are separated by spaces."]
                                      ["Lists are evaluated by default though they can be deferred with the deferrement operator."]
                                      ["An evaluated list - this will evaluate to 42: "
                                       [:br]
                                       (code-tags "(+ 20 22)")]
                                      ["A deferred list, stores expressoins without evaluating them: "
                                       [:br]
                                       (code-tags "'(true \"a str\" 100 )")]])]}

                     {:name "Values"
                      :body [ "Values consist of "
                             (code-tags "integers, doubles strings and booleans")
                             (lister [["Integers: any numeric value without a decimal will be of type integer."
                                       [:br]
                                       "Example: " (code-tags "25")]
                                      ["Double: any numeric value using a decimal will be of type double."
                                       [:br]
                                       "Example: " (code-tags "25.5") " or: " (code-tags "25.0")]
                                      ["Strings: any set of characters contained between two " (code-tags "\"") "."
                                       [:br]
                                       "Example: " (code-tags "\"This is an example str\"")]
                                      ["Boolean: " (code-tags "true false")]])]}

                     {:name "Symbols"
                      :body ["Symbols are names by which expressions are referenced."
                             (lister [["Symbols consist of upper and lowercase letters, as well as the following characters: "
                                       [:br]
                                       (code-tags ":!_%=?-+*")]
                                      ["Symbol referencing the builtin addition expression: "
                                       [:br]
                                       (code-tags "+")]
                                      ["Symbol referencing user defined expression: "
                                       [:br]
                                       (code-tags "my-lambda")]])]}

                     {:name "Deferment"
                      :body ["Deferment prevents the symbols or values in the deferred expression from being evaluated."
                             (lister [["A single quote is used to defer evaluation: " (code-tags "'")]
                                      ["Expressions deferred can be evaluated later with the " (code-tags "eval") " builtin."]
                                      ["Example deferred list: " (code-tags "'( 31 true )")]])]}])


(def builtin-sections
  [{:name "Add"
    :body [(lister [(symbols "+" "add")
                    (desc "adds all operands together."
                     [:br]
                     "Returns operand if only one provided.")
                    (arity "1+")
                    (expr-result "(+ 2 4 6)" "12")
                    (expr-result "(+ 42 )" "42")])]}
   {:name "Subtract"
    :body [(lister [(symbols "-" "sub")
                    (desc "subtracts the operands sequentially from the first."
                     [:br]
                     "Negates operand if only one provided.")
                    (arity "1+")
                    (expr-result "(- 5  1 -10)" "14")
                    (expr-result "(- 1)" "-1")])]}
   {:name "Multiply"
    :body [(lister [(symbols "*" "mul")
                    (desc "multiplies all operands together."
                     [:br]
                     "Returns operand if only one provided.")
                    (arity "1+")
                    (expr-result "(* 10 20 30)" "6000")
                    (expr-result "(* 10)" "10")])]}
   {:name "Divide"
    :body [(lister [(symbols "/" "div")
                    (desc "divides operands sequentially from the first."
                     [:br]
                     "Divides 1 by operand if only one provided.")
                    (arity "1+")
                    (expr-result "(/ 42 2)" "21")
                    (expr-result "(/ 20)" "0.050000")])]}
   {:name "Modulo"
    :body [(lister [(symbols "%" "mod")
                    (desc "returns the integer remainder of the first operand divided by the second.")
                    (arity 2)
                    (expr-result "(% 5 3)" "2")])]}
   {:name "Equals"
    :body [(lister [(symbols "=")
                    (desc "checks if all operands have equal value."
                          "It is not implemented for strings yet.")
                    (arity "1+")
                    (expr-result "(= true 12)" "false")
                    (expr-result "(= 12.0 12)" "true")])]}
   {:name "Car"
    :body [(lister [(symbols "car")
                    (desc "returns first element of a list."
                          [:br]
                          "If list has no elements returns nil.")
                    (arity 1)
                    (expr-result "(car '('()))" "()")
                    (expr-result "(car '())" "nil")
                    (expr-result "(car '(1 2))" "1")])]}
   {:name "Cdr"
    :body [(lister [(symbols "cdr")
                    (desc "returns elements of list after first element.")
                    (arity 1)
                    (expr-result "(cdr '())" "nil")
                    (expr-result "(cdr '(1))" "nil")
                    (expr-result "(cdr '(1 2 3))" "(2 3)")])]}
   {:name "Cons"
    :body [(lister [(symbols "cons")
                    (desc "takes a list and an item, and returns a new list with the item appended")
                    (arity 2)
                    (expr-result "(cons 1 '())" "(1)")
                    (expr-result "(cons '() '(1)))" "(() 1)")])]}
   {:name "List"
    :body [(lister [(symbols "list")
                    (desc "returns a list containing its arguments")
                    (arity "0+")
                    (expr-result "(list )" "()")
                    (expr-result "(list 1 2 3)" "(1 2 3)")])]}
   {:name "Atom?"
    :body [(lister [(symbols "atom?")
                    (desc "returns true if value does not contain elements inside it."
                          [:br]
                          "As in, it returns true if the value is not a list, or is an empty list.")
                    (arity 1)
                    (expr-result "(atom? '())" "true")
                    (expr-result "(atom? 1)" "true")
                    (expr-result "(atom? '(1))" "false")])]}
   {:name "List?"
    :body [(lister [(symbols "list?")
                    (desc "returns true if operand is a list, and false if not.")
                    (arity 1)
                    (expr-result "(list? '(1))" "true")
                    (expr-result "(list? '())" "true")
                    (expr-result "(list? 1)" "false")])]}
   {:name "Eval"
    :body [(lister [(symbols "eval")
                    (desc "evaluates the arg passed in, will evaluate deferred lists."
                          [:br]
                          "Note: This implementaion currently evaluates list of args as well.")
                    (arity 1)
                    (expr-result "(eval '(+ 30 40))" "70")
                    (expr-result "(eval + 1 3 4 1 2 34 42)" "87")])]}
   {:name "If"
    :body [(lister [(symbols "if")
                    (desc "checks if the first expression is not "
                          (code-tags "false") " or " (code-tags "nil")
                          ".  If it's not false it returns the second expression."
                          "If it is false it returns the third expression."
                          "If it s false and the third expression is not provided it returns nil.")
                    (arity "2 or 3")
                    (expr-result "(if true true false)" "true")
                    (expr-result "(if nil true false)" "false")
                    (expr-result "(if 5 true)" "true")
                    (expr-result "(if false true)" "nil")])]}
   {:name "And"
    :body [(lister [(symbols "and")
                    (desc "returns true if all arguments are true."
                          [:br]
                          "Returns first " (code-tags "false")
                          " or " (code-tags "nil") " if one exists in argument expressions."
                          [:br]
                          "Returns false if no arguments are true.")
                    (arity "1+")
                    (expr-result "(and 1)" "1")
                    (expr-result "(and 1 true nil 5)" "nil")])]}
   {:name "Or"
    :body [(lister [(symbols "or")
                    (desc "returns first true argument."
                          [:br]
                          "If there are no true arguments returns last argument")
                    (arity "1+")
                    (expr-result "(or false 1 nil)" "1")
                    (expr-result "(or false nil)" "nil")])]}
   {:name "Let"
    :body [(lister [(symbols "let")
                    (desc "evaluates an expression with variables that exist only through the evaluation of the list the let is in."
                          [:br]
                          "It takes a list of variable value pairs, followed by an expression to evaluate."
                          [:br]
                          "As this creates a new scope, predeclared variable names can be used in let/lambda.")
                    (arity 2)
                    (expr-result "(let '(a 20 b 40) + a b)" "60")])]}
   {:name "Define"
    :body [(lister [(symbols "define")
                    (desc "associates an expression with a variable."
                          [:br]
                          "If this is done within a "
                          (code-tags "let") "or" (code-tags "lambda")
                          "then it will only persist through that scope."
                          [:br]
                          "Otherwise this will persist through the session."
                          [:br]
                          "Variables cannot be redeclared.")
                    (arity 2)
                    (expr-result "(define a 1)" "nil")
                    (expr-result "(eval a)" "1")])]}
   {:name "Lambda"
    :body [(lister [(symbols "lambda")
                    (desc "creates a function, taking in a list of argument/value pairs, and a list of expressions."
                          [:br]
                          "It returns an expression that takes arguments corresponding to the arg/value list.")
                    (arity 2)
                    (expr-result "((lambda '(a b) '(+ a b)) 5 10)" "15")
                    (expr-result "(define func (lambda '(a b) '(+ a b)))" "nil")
                    (expr-result "(func 20 30)" "50")])]}
   {:name "Quit"
    :body [(lister [(symbols "quit")
                    (desc "Exits the interpreter")
                    (arity)
                    (expr-result "(quit)" "Ending session.\nGoodbye.")])]}])


(def example-lines [
                    "./bin/crispy"
                    "Crispy lisp interpreter.  Type (quit) to exit."
                    "crispy> (define nil? (lambda '(x) '(if (= nil x) true false)))"
                    "nil"
                    "crispy> (nil? 5)"
                    "false"
                    "crispy> (nil? nil)"
                    "true"
                    "crispy> (define true? (lambda '(x) '(if x true false)))"
                    "nil"
                    "crispy> (true? 1)"
                    "true"
                    "crispy> (true? nil)"
                    "false"
                    "crispy> (true? true)"
                    "true"
                    "crispy> (define false? (lambda '(x) '(if x false true)))"
                    "nil"
                    "crispy> (false? 1)"
                    "false"
                    "crispy> (false? nil)"
                    "true"
                    "crispy> (false? false)"
                    "true"
                    "crispy> (define range (lambda '(r) '(let '(b (lambda '(c d) '(if (= c 0) d (b (- c 1 ) (cons c d )) )))  b r '())))"
                    "nil"
                    "crispy> (range 25)"
                    "(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)"
                    "crispy> ((lambda '() '( + 1 4)))"
                    "5"
                    "crispy> (or false false nil)"
                    "nil"
                    "crispy> (or false  \"grr\" 1 true)"
                    "\"grr\""
                    "crispy> (define b \"rarr\")"
                    "nil"
                    "crispy> (= \"grr\" b)"
                    "false"
                    "crispy> (define c 1)"
                    "nil"
                    "crispy> (and true c 3 false 4)"
                    "false"
                    "crispy> (and true c 3)"
                    "3"
                    "crispy> ^D"
                    "Ending session."
                    "Goodbye."])


(def example-sections [
                       {:name "Example usage"
                        :body (code-block example-lines)}])




(def top-links [:div
                [:a {:href "/"} "Welcome"] "\t"
                [:a {:href "/builtins"} "Builtin Functions"] "\t"
                [:a {:href "/examples"} "Examples"]])

(def welcome [:div
              {:style {:background-color "lightgreen"
                       :height "100%"}}
              [:h2 "Welcome!"]
              top-links
              [:br]])

(def builtins [:div
               {:style {:background-color "lightblue"
                        :height "100%"}}
               [:h2 "Builtin Functions"]
               top-links
               [:br]])

(def example-usage [:div
                    {:style {:background-color "lightgreen"
                             :height "100%"}}
                    [:h2 "Examples"]
                    top-links
                    [:br]])

;; -------------------------
;; Views

(defn home-page []
  (vec (concat welcome (list-sections about-sections))))

(defn builtins-page []
  (vec (concat builtins (list-sections builtin-sections))))

(defn example-page []
  (vec (concat example-usage (list-sections example-sections))))

;; -------------------------
;; Routes

(defonce page (atom #'home-page))

(defn current-page []
  [:div [@page]])

(secretary/defroute "/" []
  (reset! page #'home-page))

(secretary/defroute "/builtins" []
  (reset! page #'builtins-page))

(secretary/defroute "/examples" []
  (reset! page #'example-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))

;; -------------------------
;; helper functions and content..



