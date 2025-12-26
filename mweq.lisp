;;; -*- mode: Lisp; coding: utf-8  -*-

(bcl::in-sub-package)


;;;; ==========================================================================
;;;; MWEQ-Library Core (Module 1)
;;;; Purpose: Basic Protocol for Differance Intelligence and Matchers
;;;; ==========================================================================

;;; ---------------------------------------------------------------------------
;;; 1. Protocols (The Middle-Way Equality)
;;; ---------------------------------------------------------------------------

(defclass matcher () ()
  (:documentation "Base class for all matchers in Differance Intelligence Framework."))


(defgeneric >< (matcher a b)
  (:documentation "Middle-Way Equality: Decides if A and B are equivalent under MATCHER."))


(defgeneric uncons (matcher collection)
  (:documentation "Decomposes COLLECTION into (item . rest) pairs based on MATCHER's logic."))


;;; ---------------------------------------------------------------------------
;;; 2. Basic Matcher Implementations
;;; ---------------------------------------------------------------------------

;; Something Matcher: Matches anything (Wildcard logic)
(defclass something-matcher (matcher) ())


(defmethod >< ((ctx something-matcher) a b) t)


;; Value Matcher (Shiki-Truth): Standard equality
(defmethod >< ((ctx matcher) a b)
  "Default fallback to standard equality (Shiki-Truth)."
  (equal a b))


;;; ---------------------------------------------------------------------------
;;; 3. Multiset Matcher (The Heart of Combinatorics)
;;; ---------------------------------------------------------------------------

(defclass multiset-matcher (matcher) 
  ((element-matcher :initform (make-instance 'matcher) :accessor element-matcher)))


(defmethod uncons ((ctx multiset-matcher) list)
  "同じ値を持つ要素は、1つの『可能性』としてのみ現成する（爆発防止）"
  (let ((seen nil))
    (loop :for x :in list
          :for i :from 0
          :unless (member x seen :test (lambda (a b) (>< (element-matcher ctx) a b)))
            :collect (progn
                       (push x seen)
                       (cons x (append (subseq list 0 i) (subseq list (1+ i))))))))


(defmethod >< ((ctx multiset-matcher) a b)
  ">< for Multisets: 点対（牌）が含まれていても正しく動作するように修正"
  (cond
   ;; 両方が（正当なリストまたは点対のリストである）コンスセルの場合
   ((and (typep a 'cons) (typep b 'cons))
    ;; そもそも A と B が「一つの牌」を表す点対である場合（例: (man . 1)）
    ;; それは multiset としての比較ではなく、要素としての比較を行う
    (if (and (not (typep (cdr a) 'list))
             (not (typep (cdr b) 'list)))
        (equal a b)
        ;; 集合としての比較
        (let ((list-a (copy-list a))
              (list-b (copy-list b)))
          (and (= (safe-length list-a) (safe-length list-b))
               (loop :for item :in list-a
                     :always (let ((pos (position item list-b 
                                                  :test (lambda (x y) (>< (element-matcher ctx) x y)))))
                               (when pos
                                 (setf list-b (append (subseq list-b 0 pos)
                                                      (subseq list-b (1+ pos))))
                                 t)))))))
   ;; アトム同士、または一方がアトムの場合
   (t (equal a b))))


(defun safe-length (x)
  "点対リストであっても、要素の数を返す安全な length"
  (cond ((null x) 0)
        ((atom x) 1)
        (t (loop :for curr := x :then (cdr curr)
                 :while (typep curr 'cons)
                 :count 1))))


;;; ---------------------------------------------------------------------------
;;; 4. Set Matcher (The Power Set Logic)
;;; ---------------------------------------------------------------------------

(defclass set-matcher (matcher) ())


(defmethod uncons ((ctx set-matcher) list)
  "Set uncons: Any element can be the 'head', and it stays in the 'rest' (potential)."
  (loop :for x :in list
        :collect (cons x list)))


(defmethod >< ((ctx set-matcher) a b)
  ">< for Sets: Checks if elements overlap regardless of frequency."
  (and (subsetp a b :test #'equal)
       (subsetp b a :test #'equal)))


;;; ---------------------------------------------------------------------------
;;; 5. List Matcher (The Sequential Order)
;;; ---------------------------------------------------------------------------

(defclass list-matcher (matcher) ())


(defmethod uncons ((ctx list-matcher) list)
  "List uncons: Only the actual CAR can be the head."
  (when list
    (list (cons (car list) (cdr list)))))


;;;; ==========================================================================
;;;; ><-Library Pattern Engine (Module 2) - OS v4.3
;;;; Purpose: Non-linear pattern matching and backtracking exploration
;;;; ==========================================================================

;;; ---------------------------------------------------------------------------
;;; 1. Environment Handling (Binding Context)
;;; ---------------------------------------------------------------------------

(defun extend-env (var val env matcher)
  "環境に変数 var = val を追加する。既にある場合は >< で位相同期を確認する。"
  (let ((existing (assoc var env)))
    (if existing
        ;; 非線形パターンの同期チェック (Shiki-Truth vs Ku-Truth)
        (if (>< matcher (cdr existing) val)
            (list env) ; 同期成功：現在の環境を維持
            nil)       ; 同期失敗：この枝は消滅 (Fuss)
        (list (cons (cons var val) env)))))

;;; ---------------------------------------------------------------------------
;;; 2. Core Matcher: match-all
;;; ---------------------------------------------------------------------------

(defun match-all (matcher pattern target env)
  "パターンとターゲットを照合し、可能性のあるすべての環境リストを返す。"
  (typecase pattern
    ;; 1. パターンが変数の場合 (シンボル)
    (symbol (extend-env pattern target env matcher))
    ;; 2. パターンが定数の場合 (数値やキーワードなど)
    (atom (if (>< matcher pattern target)
              (list env)
              nil))

    ;; 3. パターンがリスト構造 (p-car . p-cdr) の場合
    (list (let ((p-car (car pattern))
                (p-cdr (cdr pattern)))
            (loop :for (t-car . t-cdr) :in (uncons matcher target)
                  :append (let ((car-envs (match-all matcher p-car t-car env)))
                            (loop for next-env in car-envs
                                  append (match-all matcher p-cdr t-cdr next-env))))))
    ;; 4. その他 (想定外の位相)
    (T nil)))


;;; ---------------------------------------------------------------------------
;;; 3. Utility: Pattern Extraction
;;; ---------------------------------------------------------------------------

(defun match-one (matcher pattern target)
  "最初の解だけを抽出する（決定論的な観測）。"
  (car (match-all matcher pattern target nil)))


;;; ---------------------------------------------------------------------------
;;; 4. Example: Manual trace of non-linear matching
;;; ---------------------------------------------------------------------------
;; (match-all (make-instance 'multiset-matcher) '(x x . rest) '(1 2 2 1) nil)
;; この呼び出しは Module 1 の uncons と Module 2 の extend-env を同期させ、
;; 重複する 'x' が同じ値（1=1 または 2=2）になる解のみを現成します。



;;;; ==========================================================================
;;;; ><-Library Macros (Module 3) - OS v4.3
;;;; Purpose: User-friendly syntax for >< matching (><match, ><let)
;;;; ==========================================================================

;;; ---------------------------------------------------------------------------
;;; 1. Helper: Extraction of values from Environment
;;; ---------------------------------------------------------------------------

(defun get-bound-values (vars env)
  "環境(env)から指定された変数リスト(vars)に対応する値を取り出す。"
  (mapcar (lambda (v) (cdr (assoc v env))) vars))


;;; ---------------------------------------------------------------------------
;;; 2. The ><match Macro
;;; ---------------------------------------------------------------------------

(defmacro ><match (target matcher-form &body clauses)
  "(><match data (make-instance 'multiset-matcher)
     ((x x . rest) (list x rest)))"
  (let ((g-target (gensym "TARGET"))
        (g-matcher (gensym "MATCHER"))
        (g-env-list (gensym "ENV-LIST")))
    `(let ((,g-target ,target)
           (,g-matcher ,matcher-form))
       (append
        ,@(loop :for (pattern result-form) :in clauses
                :collect `(let ((,g-env-list (match-all ,g-matcher ',pattern ,g-target nil)))
                            (loop :for env :in ,g-env-list
                                  :collect (let ,(loop :for var :in (flatten-pattern pattern)
                                                       :collect `(,var (cdr (assoc ',var env))))
                                             (declare (ignorable ,@(flatten-pattern pattern)))
                                             ,result-form))))))))


(defun flatten-pattern (pattern)
  "パターンからシンボル（変数）のみを抽出する補助関数。"
  (typecase pattern
    (null nil)
    (symbol (list pattern))
    (list (append (flatten-pattern (car pattern))
                  (flatten-pattern (cdr pattern))))
    (T nil)))


;;; ---------------------------------------------------------------------------
;;; 3. The ><let Macro (Non-deterministic binding)
;;; ---------------------------------------------------------------------------

(defmacro ><let ((pattern target matcher-form) &body body)
  "非決定的な束縛。複数の解がある場合、それらすべてに対してbodyを実行する。"
  `(><match ,target ,matcher-form
     (,pattern (progn ,@body))))


;;; ---------------------------------------------------------------------------
;;; 4. Shortcut Functions
;;; ---------------------------------------------------------------------------

(declaim (inline m-list m-mset m-set))


(defun m-list () (load-time-value (make-instance 'list-matcher)))


(defun m-mset () (load-time-value (make-instance 'multiset-matcher)))


(defun m-set () (load-time-value (make-instance 'set-matcher)))


;;;; ==========================================================================
;;;; ><-Library Utils (Module 4) - OS v4.3
;;;; Purpose: Standard library for Ku-Mathematics (Sudoku, Nonograms, etc.)
;;;; ==========================================================================

;;; ---------------------------------------------------------------------------
;;; 1. Combinatorial Utilities (The difw realization)
;;; ---------------------------------------------------------------------------

(defun combinations (k list)
  "K個の組み合わせを現成する。"
  (><match list (m-list) ;; リストの順序を維持しつつ選択
    ((x . rest) (if (= k 1) 
                    (list (list x))
                    (mapcar (lambda (c) (cons x c)) 
                            (combinations (1- k) rest))))
    ((_ . rest) (combinations k rest))))


(defun permutations (k list)
  "K個の順列を現成する。"
  (if (zerop k)
      (list nil)
      (><match list (m-mset) ;; 多重集合としてどこからでも抜く
        ((x . rest) (mapcar (lambda (p) (cons x p)) 
                            (permutations (1- k) rest))))))


;;; ---------------------------------------------------------------------------
;;; 2. Structural Analysis (group-by & extract-constraints)
;;; ---------------------------------------------------------------------------

(defun group-by (line &key (test #'equal))
  "連続する同一要素を位相（Coherence）としてまとめる。"
  (when line
    (let ((first-val (car line)))
      (multiple-value-bind (group rest)
                           (loop :for x :in line
                                 :while (funcall test x first-val) :collect x :into g
                                 :finally (return (values g (nthcdr (length g) line))))
        (cons group (group-by rest :test test))))))





;;; *EOF*
