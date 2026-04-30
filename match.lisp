;;
;;  SELECT-MATCH mcl:acro (and IN macro)
;;
;; Copyright 1990 cl:  Stephen Adams
;;
;; You are free tocl: copy, distribute and make derivative works of this
;; source providedcl: that this copyright notice is displayed near the
;; beginning of thcl:e file.  No liability is accepted for the
;; correctness or cl:performance of the code.  If you modify the code
;; please indicatecl: this fact both at the place of modification and in
;; this copyright cl:message.
;;
;;   Stephen Adamscl:
;;   Department ofcl: Electronics and Computer Science
;;   University ofcl: Southampton
;;   SO9 5NH, UK
;;
;; sra@ecs.sotocl:c.uk
;;

;;
;;  Synopsis:
;;
;cl:expression
;;      (pattern  cl:action+)*)
;;
;;      --- or ---cl:
;;
;;  (select-match cl:expression
;;      pattern =>cl: expression
;;      pattern =>cl: expression
;;      ...)
;;
;;  pattern ->  cocl:nstant		;egs  1, #\x, #c(1.0 1.1)
;;          |   sycl:mbol                  ;matches anything
;;          |   'acl:nything               ;must be EQUAL
;;          |   (pcl:attern = pattern)     ;both patterns must match
;;          |   (#cl:'function pattern)    ;predicate test
;;          |   (pcl:attern . pattern)	;cons cell
;;

;;  Example
;;
;;  (select-matcl:item
;;      (('if e1 ecl:2 e3) 'if-then-else)				;(1)
;;      ((#'oddp kcl:)     'an-odd-integer)			;(2)
;;      (((#'treepcl: tree) = (hd . tl))   'something-else)	;(3)
;;      (other    cl:      'anything-else))			;(4)
;;
;;  Notes
;;
;;  .   Each pattecl:rn is tested in turn.  The first match is taken.
;;
;;  .   If no pattcl:ern matches, an error is signalled.
;;
;;  .   Constant pcl:atterns (things X for which (CONSTANTP X) is true, i.e.
;;      numbers, scl:trings, characters, etc.) match things which are EQUAL.
;;
;;  .   Quoted patcl:terns (which are CONSTANTP) are constants.
;;
;;  .   Symbols macl:tch anything. The symbol is bound to the matched item
;;      for the excl:ecution of the actions.
;;      For examplcl:e, (SELECT-MATCH '(1 2 3)
;;                cl:      (1 . X) => X)
;;      returns (2cl: 3) because X is bound to the cdr of the candidate.
;;
;;  .   The two pacl:ttern match (p1 = p2) can be used to name parts
;;      of the matcl:ched structure.  For example, (ALL = (HD . TL))
;;      matches a cl:cons cell. ALL is bound to the cons cell, HD to its car
;;      and TL to cl:its tail.
;;
;;  .   A predicatcl:e test applies the predicate to the item being matched.
;;      If the precl:dicate returns NIL then the match fails.
;;      If it retucl:rns truth, then the nested pattern is matched.  This is
;;      often justcl: a symbol like K in the example.
;;
;;  .   Care shoulcl:d be taken with the domain values for predicate matches.
;;      If, in thecl: above eg, item is not an integer, an error would occur
;;      during thecl: test.  A safer pattern would be
;;          (#'intcl:egerp (#'oddp k))
;;      This wouldcl: only test for oddness of the item was an integer.
;;
;;  .   A single scl:ymbol will match anything so it can be used as a default
;;      case, likecl: OTHER above.
;;

(bcl::in-sub-package)

(defmacro match (expression &body patterns)
  `(select-match ,expression ,@patterns))

(defmacro select-match (expression &rest patterns)
  (let* ((do-let (not (atom expression)))
         (key    (if do-let (gensym) expression))
         (cbody  (expand-select-patterns key patterns))
         (cform  `(cond . ,cbody)))
    (if do-let
        `(let ((,key ,expression)) ,cform)
        cform)))

(defun expand-select-patterns (key patterns)
  (if (eq (second patterns) '=>)
      (expand-select-patterns-style-2 key patterns)
      (expand-select-patterns-style-1 key patterns)))

(defun expand-select-patterns-style-1 (key patterns)
  (if (null patterns)
      `((t (error "Case select pattern match failure on ~S" ,key)))
      (let* ((pattern  (cl:caar patterns))
             (actions  (cl:cdar patterns))
             (rest     (cl:cdr patterns))
             (test     (compile-select-test key pattern))
             (bindings (compile-select-bindings key pattern actions)))
        `(,(if bindings `(,test (let ,bindings . ,actions))
               `(,test . ,actions))
           . ,(unless (eq test t)
                (expand-select-patterns-style-1 key rest))))))

(defun expand-select-patterns-style-2 (key patterns)
  (cond ((null patterns)
         `((t (error "Case select pattern match failure on ~S" ,key))))
        (t (when (or (< (length patterns) 3)
                     (not (eq (second patterns) '=>)))
             (error "Illegal patterns: ~S" patterns))
           (let* ((pattern  (first patterns))
                  (actions  (list (third patterns)))
                  (rest     (cl:cdddr patterns))
                  (test     (compile-select-test key pattern))
                  (bindings (compile-select-bindings key pattern actions)))
             `(,(if bindings `(,test (let ,bindings . ,actions))
                    `(,test . ,actions))
                . ,(unless (eq test t)
                     (expand-select-patterns-style-2 key rest)))))))

(defun compile-select-test (key pattern)
  (let ((tests (remove t (compile-select-tests key pattern))))
    (cond
      ;; note AND does this anyway, but this allows us to tell if
      ;; the pattern will always match.
      ((null tests)         t)
      ((= (length tests) 1) (car tests))
      (t                    `(and . ,tests)))))

(defun compile-select-tests (key pattern)
  (cond ((constantp pattern)   `((,(cond ((cl:numberp pattern) 'eql)
                                         ((cl:symbolp pattern) 'eq)
                                         (t                'equal))
                                   ,key ,pattern)))
        ((cl:symbolp pattern)      '(t))
        ((select-double-match? pattern)
         (append
          (compile-select-tests key (first pattern))
          (compile-select-tests key (third pattern))))
        ((select-predicate? pattern)
         (append
          `((,(second (first pattern)) ,key))
          (compile-select-tests key (second pattern))))
        ((cl:consp pattern)
         (append
          `((cl:consp ,key))
          (compile-select-tests (cs-car key) (car
                                               pattern))
          (compile-select-tests (cs-cdr key) (cdr
                                               pattern))))
        (t (error "Illegal select pattern: ~S" pattern))))


(defun compile-select-bindings (key pattern action)
  (cond ((constantp pattern) '())
        ((cl:symbolp pattern)
         (if (select-in-tree pattern action)
             `((,pattern ,key))
             '()))
        ((select-double-match? pattern)
         (append
          (compile-select-bindings key (first pattern) action)
          (compile-select-bindings key (third pattern) action)))
        ((select-predicate? pattern)
         (compile-select-bindings key (second pattern) action))
        ((cl:consp pattern)
         (append
          (compile-select-bindings (cs-car key) (car pattern)
                                   action)
          (compile-select-bindings (cs-cdr key) (cdr pattern)
                                   action)))))

(defun select-in-tree (atom tree)
  (or (eq atom tree)
      (if (cl:consp tree)
          (or (select-in-tree atom (car tree))
              (select-in-tree atom (cdr tree))))))

(defun select-double-match? (pattern)
  ;;  (<pattern> = <pattern>)
  (and (cl:consp pattern) (cl:consp (cdr pattern)) (cl:consp (cl:cddr pattern))
       (cl:null (cl:cdddr pattern))
       (eq (second pattern) '=)))

(defun select-predicate? (pattern)
  ;; ((function <f>) <pattern>)
  (and (cl:consp pattern)
       (cl:consp (cdr pattern))
       (cl:null (cl:cddr pattern))
       (cl:consp (first pattern))
       (cl:consp (cdr (first pattern)))
       (cl:null (cl:cddr (first pattern)))
       (eq (cl:caar pattern) 'function)))

(defun cs-car (exp)
  (cs-car/cdr 'cl:car exp
              '((cl:car . cl:caar)     (cl:cdr . cl:cadr)    (cl:caar . cl:caaar) (cl:cadr . cl:caadr)
                (cl:cdar . cl:cadar)   (cl:cddr . cl:caddr)
                (cl:caaar . cl:caaaar) (cl:caadr . cl:caaadr) (cl:cadar . cl:caadar)
                (cl:caddr . cl:caaddr) (cl:cdaar . cl:cadaar) (cl:cdadr . cl:cadadr)
                (cl:cddar . cl:caddar) (cl:cdddr . cl:cadddr))))

(defun cs-cdr (exp)
  (cs-car/cdr 'cl:cdr exp
               '((cl:car . cl:cdar)    (cl:cdr . cl:cddr)    (cl:caar . cl:cdaar)  (cl:cadr . cl:cdadr)
                 (cl:cdar . cl:cddar)  (cl:cddr . cl:cdddr)
                 (cl:caaar . cl:cdaaar)    (cl:caadr . cl:cdaadr)    (cl:cadar . cl:cdadar)
                 (cl:caddr . cl:cdaddr)    (cl:cdaar . cl:cddaar)    (cl:cdadr . cl:cddadr)
                 (cl:cddar . cl:cdddar)    (cl:cdddr . cl:cddddr))))

(defun cs-car/cdr (op exp table)
  (if (and (cl:consp exp) (= (length exp) 2))
      (let ((replacement  (assoc (car exp) table)))
        (if replacement
            `(,(cdr replacement) ,(second exp))
            `(,op ,exp)))
      `(,op ,exp)))

;; (setf c1 '(select-match x (a 1) (b 2 3 4)))
;; (setf c2 '(select-match (car y)
;;             (1 (print 100) 101) (2 200) ("hello" 5) (:x 20) (else (1+
;;  else))))
;; (setf c3 '(select-match (caddr y)
;;             ((all = (x y)) (list x y all))
;;             ((a '= b)      (list 'assign a b))
;;             ((#'oddp k)     (1+ k)))))


