;;;; bcl.lisp

(cl:defpackage bcl.internal
  (:use cl ppcre))


(cl:in-package bcl.internal)


#++
(loop :for s :being :the :external-symbols :of :series
      :collect (make-symbol (string s)))


(defpackage #:bcl
  (:use #:c2cl #:series)
  (:shadow #:get #:set)
  (:shadowing-import-from #:nil #:let)
  (:export name)
  (:export for)
  (:export
   ;; seq.lisp
   mem
   fin)
  (:export
   w/infile
   w/package-iterator
   w/standard-io-syntax
   w/compilation-unit
   w/outstring
   w/instring
   w/accessors
   w/hash-table-iterator
   w/slots
   w/outfile
   w/condition-restarts
   w/stream
   w/simple-restart)
  (:export for)
  (:export
   *bcl*
   bcl-syntax
   set
   loop
   a
   isa
   eval-always
   ^
   fun
   let
   get
   put
   prop
   then
   else)
  (:export 
   #:<arithmetic-error>
   #:<array>
   #:<bignum>
   #:<bit-vector>
   #:<broadcast-stream>
   #:<built-in-class>
   #:<cell-error>
   #:<character>
   #:<class>
   #:<complex>
   #:<concatenated-stream>
   #:<condition>
   #:<cons>
   #:<control-error>
   #:<direct-slot-definition>
   #:<division-by-zero>
   #:<double-float>
   #:<echo-stream>
   #:<effective-slot-definition>
   #:<end-of-file>
   #:<eql-specializer*>
   #:<error>
   #:<file-error>
   #:<file-stream>
   #:<fixnum>
   #:<float>
   #:<floating-point-inexact>
   #:<floating-point-invalid-operation>
   #:<floating-point-overflow>
   #:<floating-point-underflow>
   #:<forward-referenced-class>
   #:<funcallable-standard-class>
   #:<funcallable-standard-object>
   #:<function>
   #:<generic-function>
   #:<hash-table>
   #:<integer>
   #:<list>
   #:<logical-pathname>
   #:<metaobject>
   #:<method-combination>
   #:<method>
   #:<null>
   #:<number>
   #:<package-error>
   #:<package>
   #:<parse-error>
   #:<pathname>
   #:<print-not-readable>
   #:<program-error>
   #:<random-state>
   #:<ratio>
   #:<rational>
   #:<reader-error>
   #:<readtable>
   #:<real>
   #:<restart>
   #:<sequence>
   #:<serious-condition>
   #:<short-float>
   #:<simple-array>
   #:<simple-base-string>
   #:<simple-bit-vector>
   #:<simple-condition>
   #:<simple-error>
   #:<simple-string>
   #:<simple-type-error>
   #:<simple-vector>
   #:<simple-warning>
   #:<single-float>
   #:<slot-definition>
   #:<specializer>
   #:<standard-accessor-method>
   #:<standard-class>
   #:<standard-direct-slot-definition>
   #:<standard-effective-slot-definition>
   #:<standard-generic-function>
   #:<standard-method>
   #:<standard-object>
   #:<standard-reader-method>
   #:<standard-slot-definition>
   #:<standard-writer-method>
   #:<storage-condition>
   #:<stream-error>
   #:<stream>
   #:<string-stream>
   #:<string>
   #:<structure-class>
   #:<structure-object>
   #:<style-warning>
   #:<symbol>
   #:<synonym-stream>
   #:<t>
   #:<two-way-stream>
   #:<type-error>
   #:<unbound-slot>
   #:<unbound-variable>
   #:<undefined-function>
   #:<vector>
   #:<warning>)
  (:export
   &allow-other-keys
   &aux
   &body
   &environment
   &key
   &optional
   &rest
   &whole
   *
   **
   ***
   *break-on-signals*
   *compile-file-pathname*
   *compile-file-truename*
   *compile-print*
   *compile-verbose*
   *debug-io*
   *debugger-hook*
   *default-pathname-defaults*
   *error-output*
   *features*
   *gensym-counter*
   *load-pathname*
   *load-print*
   *load-truename*
   *load-verbose*
   *macroexpand-hook*
   *modules*
   *package*
   *print-array*
   *print-base*
   *print-case*
   *print-circle*
   *print-escape*
   *print-gensym*
   *print-length*
   *print-level*
   *print-lines*
   *print-miser-width*
   *print-pprint-dispatch*
   *print-pretty*
   *print-radix*
   *print-readably*
   *print-right-margin*
   *query-io*
   *random-state*
   *read-base*
   *read-default-float-format*
   *read-eval*
   *read-suppress*
   *readtable*
   *standard-input*
   *standard-output*
   *terminal-io*
   *trace-output*
   +
   ++
   +++
   -
   /
   //
   ///
   /=
   1+
   1-
   <
   <=
   =
   >
   >=
   abort
   abs
   accessor-method-slot-definition
   acons
   acos
   acosh
   add-dependent
   add-direct-method
   add-direct-subclass
   add-method
   adjoin
   adjust-array
   adjustable-array-p
   allocate-instance
   alpha-char-p
   alphanumericp
   and
   append
   apply
   apropos
   apropos-list
   aref
   arithmetic-error
   arithmetic-error-operands
   arithmetic-error-operation
   array
   array-dimension
   array-dimension-limit
   array-dimensions
   array-displacement
   array-element-type
   array-has-fill-pointer-p
   array-in-bounds-p
   array-rank
   array-rank-limit
   array-row-major-index
   array-total-size
   array-total-size-limit
   ash
   asin
   asinh
   assert
   assoc
   assoc-if
   atan
   atanh
   atom
   base-char
   base-string
   bignum
   bit
   bit-and
   bit-andc1
   bit-andc2
   bit-eqv
   bit-ior
   bit-nand
   bit-nor
   bit-not
   bit-orc1
   bit-orc2
   bit-vector
   bit-xor
   block
   boole
   boole-1
   boole-2
   boole-and
   boole-andc1
   boole-andc2
   boole-c1
   boole-c2
   boole-clr
   boole-eqv
   boole-ior
   boole-nand
   boole-nor
   boole-orc1
   boole-orc2
   boole-set
   boole-xor
   boolean
   both-case-p
   boundp
   break
   broadcast-stream
   broadcast-stream-streams
   built-in-class
   butlast
   byte
   byte-position
   byte-size
   caar
   caddr
   cadr
   call-arguments-limit
   call-method
   call-next-method
   car
   case
   catch
   ccase
   cddr
   cdr
   ceiling
   cell-error
   cell-error-name
   cerror
   change-class
   char
   char-code
   char-code-limit
   char-downcase
   char-equal
   char-greaterp
   char-int
   char-lessp
   char-name
   char-not-equal
   char-not-greaterp
   char-not-lessp
   char-upcase
   char/=
   char<
   char<=
   char=
   char>
   char>=
   character
   check-type
   cis
   class
   class-default-initargs
   class-direct-default-initargs
   class-direct-slots
   class-direct-subclasses
   class-direct-superclasses
   class-finalized-p
   class-name
   class-of
   class-precedence-list
   class-prototype
   class-slots
   classp
   clear-input
   clear-output
   close
   clrhash
   code-char
   coerce
   compilation-speed
   compile
   compile-file
   compile-file-pathname
   compiled-function
   compiler-macro
   compiler-macro-function
   complement
   complex
   compute-applicable-methods
   compute-applicable-methods-using-classes
   compute-class-precedence-list
   compute-default-initargs
   compute-discriminating-function
   compute-effective-method
   compute-effective-method-function
   compute-effective-slot-definition
   compute-restarts
   compute-slots
   concatenate
   concatenated-stream
   concatenated-stream-streams
   cond
   condition
   conjugate
   cons
   constantly
   constantp
   continue
   control-error
   copy-alist
   copy-list
   copy-pprint-dispatch
   copy-readtable
   copy-seq
   copy-structure
   copy-symbol
   copy-tree
   cos
   cosh
   count
   count-if
   ctypecase
   debug
   decf
   declaim
   declaration
   declare
   decode-float
   decode-universal-time
   defclass
   defconstant
   defgeneric
   define-compiler-macro
   define-condition
   define-method-combination
   define-modify-macro
   define-setf-expander
   define-symbol-macro
   defmacro
   defmethod
   defpackage
   defparameter
   defsetf
   defstruct
   deftype
   defun
   defvar
   delete
   delete-duplicates
   delete-file
   delete-if
   delete-package
   denominator
   deposit-field
   describe
   describe-object
   destructuring-bind
   digit-char
   digit-char-p
   direct-slot-definition
   direct-slot-definition-class
   directory
   directory-namestring
   disassemble
   division-by-zero
   do
   do*
   do-all-symbols
   do-external-symbols
   do-symbols
   documentation
   dolist
   dotimes
   double-float
   double-float-epsilon
   double-float-negative-epsilon
   dpb
   dribble
   dynamic-extent
   ecase
   echo-stream
   echo-stream-input-stream
   echo-stream-output-stream
   ed
   effective-slot-definition
   effective-slot-definition-class
   eighth
   elt
   encode-universal-time
   end-of-file
   endp
   enough-namestring
   ensure-class
   ensure-class-using-class
   ensure-directories-exist
   ensure-finalized
   ensure-generic-function
   ensure-generic-function-using-class
   ensure-method
   eq
   eql
   eql-specializer
   eql-specializer*
   eql-specializer-object
   equal
   equalp
   error
   etypecase
   eval
   eval-when
   evenp
   every
   exp
   export
   expt
   extended-char
   extract-lambda-list
   extract-specializer-names
   fboundp
   fceiling
   fdefinition
   ffloor
   fifth
   file-author
   file-error
   file-error-pathname
   file-length
   file-namestring
   file-position
   file-stream
   file-string-length
   file-write-date
   fill
   fill-pointer
   finalize-inheritance
   find
   find-all-symbols
   find-class
   find-if
   find-method
   find-method-combination
   find-package
   find-restart
   find-symbol
   finish-output
   first
   fix-slot-initargs
   fixnum
   flet
   float
   float-digits
   float-precision
   float-radix
   float-sign
   floating-point-inexact
   floating-point-invalid-operation
   floating-point-overflow
   floating-point-underflow
   floor
   fmakunbound
   force-output
   format
   formatter
   forward-referenced-class
   fourth
   fresh-line
   fround
   ftruncate
   ftype
   funcall
   funcallable-standard-class
   funcallable-standard-instance-access
   funcallable-standard-object
   function
   function-keywords
   function-lambda-expression
   gcd
   generic-function
   generic-function-argument-precedence-order
   generic-function-declarations
   generic-function-lambda-list
   generic-function-method-class
   generic-function-method-combination
   generic-function-methods
   generic-function-name
   gensym
   gentemp
   get
   get-setf-expansion
   go
   graphic-char-p
   handler-bind
   handler-case
   hash-table
   hash-table-count
   hash-table-rehash-size
   hash-table-rehash-threshold
   hash-table-size
   hash-table-test
   host-namestring
   identity
   if
   ignorable
   ignore
   ignore-errors
   imagpart
   import
   in-package
   incf
   initialize-instance
   inline
   input-stream-p
   inspect
   integer
   integer-decode-float
   integer-length
   interactive-stream-p
   intern
   intern-eql-specializer
   intern-eql-specializer*
   internal-time-units-per-second
   intersection
   invalid-method-error
   invoke-debugger
   invoke-restart
   invoke-restart-interactively
   isqrt
   keyword
   labels
   lambda
   lambda-list-keywords
   lambda-parameters-limit
   last
   lcm
   ldb
   ldb-test
   ldiff
   least-negative-double-float
   least-negative-long-float
   least-negative-normalized-double-float
   least-negative-normalized-long-float
   least-negative-normalized-short-float
   least-negative-normalized-single-float
   least-negative-short-float
   least-negative-single-float
   least-positive-double-float
   least-positive-long-float
   least-positive-normalized-double-float
   least-positive-normalized-long-float
   least-positive-normalized-short-float
   least-positive-normalized-single-float
   least-positive-short-float
   least-positive-single-float
   length
   let
   let*
   lisp-implementation-type
   lisp-implementation-version
   list
   list*
   list-all-packages
   list-length
   listen
   load
   load-logical-pathname-translations
   load-time-value
   locally
   log
   logand
   logandc1
   logandc2
   logbitp
   logcount
   logeqv
   logical-pathname
   logical-pathname-translations
   logior
   lognand
   lognor
   lognot
   logorc1
   logorc2
   logtest
   logxor
   long-float
   long-float-epsilon
   long-float-negative-epsilon
   long-site-name
   loop
   loop-finish
   lower-case-p
   machine-instance
   machine-type
   machine-version
   macro-function
   macroexpand
   macroexpand-1
   macrolet
   make-array
   make-broadcast-stream
   make-concatenated-stream
   make-condition
   make-dispatch-macro-character
   make-echo-stream
   make-hash-table
   make-instance
   make-instances-obsolete
   make-list
   make-load-form
   make-load-form-saving-slots
   make-method
   make-method-lambda
   make-package
   make-pathname
   make-random-state
   make-sequence
   make-string
   make-string-input-stream
   make-string-output-stream
   make-symbol
   make-synonym-stream
   make-two-way-stream
   makunbound
   map
   map-dependents
   map-into
   mapc
   mapcan
   mapcar
   mapcon
   maphash
   mapl
   maplist
   mask-field
   max
   member
   member-if
   merge
   merge-pathnames
   metaobject
   method
   method-combination
   method-combination-error
   method-function
   method-generic-function
   method-lambda-list
   method-qualifiers
   method-specializers
   min
   minusp
   mismatch
   mod
   most-negative-double-float
   most-negative-fixnum
   most-negative-long-float
   most-negative-short-float
   most-negative-single-float
   most-positive-double-float
   most-positive-fixnum
   most-positive-long-float
   most-positive-short-float
   most-positive-single-float
   muffle-warning
   multiple-value-bind
   multiple-value-call
   multiple-value-list
   multiple-value-prog1
   multiple-value-setq
   multiple-values-limit
   name-char
   namestring
   nbutlast
   nconc
   next-method-p
   nil
   nintersection
   ninth
   no-applicable-method
   no-next-method
   not
   notany
   notevery
   notinline
   nreconc
   nreverse
   nset-difference
   nset-exclusive-or
   nstring-capitalize
   nstring-downcase
   nstring-upcase
   nsublis
   nsubst
   nsubst-if
   nsubstitute
   nsubstitute-if
   nth
   nth-value
   nthcdr
   null
   number
   numerator
   nunion
   oddp
   open
   open-stream-p
   optimize
   or
   otherwise
   output-stream-p
   package
   package-error
   package-error-package
   package-name
   package-nicknames
   package-shadowing-symbols
   package-use-list
   package-used-by-list
   pairlis
   parse-error
   parse-integer
   parse-namestring
   pathname
   pathname-device
   pathname-directory
   pathname-host
   pathname-match-p
   pathname-name
   pathname-type
   pathname-version
   peek-char
   phase
   pi
   plusp
   pop
   position
   position-if
   pprint
   pprint-dispatch
   pprint-exit-if-list-exhausted
   pprint-fill
   pprint-indent
   pprint-linear
   pprint-logical-block
   pprint-newline
   pprint-pop
   pprint-tab
   pprint-tabular
   prin1
   prin1-to-string
   princ
   princ-to-string
   print
   print-not-readable
   print-not-readable-object
   print-object
   print-unreadable-object
   probe-file
   proclaim
   prog
   prog*
   prog1
   prog2
   progn
   program-error
   progv
   provide
   psetf
   psetq
   push
   pushnew
   quote
   random
   random-state
   rassoc
   rassoc-if
   ratio
   rational
   rationalize
   read
   read-byte
   read-char
   read-char-no-hang
   read-delimited-list
   read-from-string
   read-line
   read-preserving-whitespace
   read-sequence
   reader-error
   reader-method-class
   readtable
   readtable-case
   real
   realpart
   reduce
   reinitialize-instance
   rem
   remf
   remhash
   remove
   remove-dependent
   remove-direct-method
   remove-direct-subclass
   remove-duplicates
   remove-if
   keep-if
   remove-method
   remprop
   rename-file
   rename-package
   replace
   require
   required-args
   rest
   restart
   restart-bind
   restart-case
   restart-name
   return
   return-from
   revappend
   reverse
   room
   rotatef
   round
   row-major-aref
   rplaca
   rplacd
   safety
   satisfies
   sbit
   scale-float
   schar
   search
   second
   sequence
   serious-condition
   set
   set-difference
   set-exclusive-or
   set-funcallable-instance-function
   set-pprint-dispatch
   setf
   setq
   seventh
   shadow
   shadowing-import
   shared-initialize
   shiftf
   short-float
   short-float-epsilon
   short-float-negative-epsilon
   short-site-name
   signal
   signed-byte
   signum
   simple-array
   simple-base-string
   simple-bit-vector
   simple-condition
   simple-condition-format-arguments
   simple-condition-format-control
   simple-error
   simple-string
   simple-type-error
   simple-vector
   simple-warning
   sin
   single-float
   single-float-epsilon
   single-float-negative-epsilon
   sinh
   sixth
   sleep
   slot-boundp
   slot-boundp-using-class
   slot-definition
   slot-definition-allocation
   slot-definition-initargs
   slot-definition-initform
   slot-definition-initfunction
   slot-definition-location
   slot-definition-name
   slot-definition-readers
   slot-definition-type
   slot-definition-writers
   slot-exists-p
   slot-makunbound
   slot-makunbound-using-class
   slot-missing
   slot-unbound
   slot-value
   slot-value-using-class
   software-type
   software-version
   some
   sort
   space
   special
   special-operator-p
   specializer
   specializer-direct-generic-functions
   specializer-direct-methods
   speed
   sqrt
   stable-sort
   standard
   standard-accessor-method
   standard-char
   standard-class
   standard-direct-slot-definition
   standard-effective-slot-definition
   standard-generic-function
   standard-instance-access
   standard-method
   standard-object
   standard-reader-method
   standard-slot-definition
   standard-writer-method
   step
   storage-condition
   store-value
   stream
   stream-element-type
   stream-error
   stream-error-stream
   stream-external-format
   string
   string-capitalize
   string-downcase
   string-equal
   string-greaterp
   string-left-trim
   string-lessp
   string-not-equal
   string-not-greaterp
   string-not-lessp
   string-right-trim
   string-stream
   string-trim
   string-upcase
   string/=
   string<
   string<=
   string=
   string>
   string>=
   structure
   structure-class
   structure-object
   style-warning
   subclassp
   sublis
   subseq
   subsetp
   subst
   subst-if
   substitute
   substitute-if
   subtypep
   svref
   sxhash
   symbol
   symbol-function
   symbol-macrolet
   symbol-name
   symbol-package
   symbol-plist
   symbol-value
   synonym-stream
   synonym-stream-symbol
   t
   tagbody
   tailp
   tan
   tanh
   tenth
   terpri
   the
   third
   throw
   time
   trace
   translate-logical-pathname
   translate-pathname
   tree-equal
   truename
   truncate
   two-way-stream
   two-way-stream-input-stream
   two-way-stream-output-stream
   type
   type-error
   type-error-datum
   type-error-expected-type
   type-of
   typecase
   typep
   unbound-slot
   unbound-slot-instance
   unbound-variable
   undefined-function
   unexport
   unintern
   union
   unless
   unread-char
   unsigned-byte
   untrace
   unuse-package
   unwind-protect
   update-dependent
   update-instance-for-different-class
   update-instance-for-redefined-class
   upgraded-array-element-type
   upgraded-complex-part-type
   upper-case-p
   use-package
   use-value
   user-homedir-pathname
   validate-superclass
   values
   values-list
   variable
   vector
   vector-pop
   vector-push
   vector-push-extend
   warn
   warn-on-defmethod-without-generic-function
   warning
   when
   wild-pathname-p
   with-accessors
   with-compilation-unit
   with-condition-restarts
   with-hash-table-iterator
   with-input-from-string
   with-open-file
   with-open-stream
   with-output-to-string
   with-package-iterator
   with-simple-restart
   with-slots
   with-standard-io-syntax
   write
   write-byte
   write-char
   write-line
   write-sequence
   write-string
   write-to-string
   writer-method-class
   y-or-n-p
   yes-or-no-p
   zerop)
  ;; re
  (:export 
   #:re.apropos-list
   #:re.apropos
   #:re.replace
   #:re.replace-all
   #:re.ppcre-syntax-error-pos
   #:re.parse-tree-synonym
   #:re.do-register-groups
   #:re.do-scans
   #:re.split
   #:re.all-matches
   #:re.ppcre-syntax-error-string
   #:re.define-parse-tree-synonym
   #:re.scan-to-strings
   #:re.register-groups-bind
   #:re.do-matches
   #:re.create-optimized-test-function
   #:re.parse-string
   #:re.do-matches-as-strings
   #:re.all-matches-as-strings
   #:re.scan
   #:re.create-scanner
   #:re.quote-meta-chars)
  (:export 
   #:*allow-quoting*
   #:*property-resolver*
   #:*look-ahead-for-suffix*
   #:*allow-named-registers*
   #:*optimize-char-classes*
   #:*regex-char-code-limit*
   #:*use-bmh-matchers*)
  (:export
   ;; series
   #:collecting-fn
   #:collect-length
   #:to-alter
   #:optimizable-series-function
   #:scan-multiple
   #:gatherlet
   #:collect-nconc
   #:scan-sublists
   #:fgatherlet
   #:off-line-port
   #:generator
   #:gather-next
   #:gathering
   #:collect-file
   #:collect-alist
   #:collect-plist
   #:collect-first
   #:scan-alist
   #:scan-stream
   #:producing
   #:scan-range
   #:collect-last
   #:encapsulated
   #:series
   #:series-element-type
   #:collect-hash
   #:collect-fn
   #:indefinite-extent
   #:collect-and
   #:collect-nth
   #:collect-or
   #:collect-append
   #:collect
   #:fgathering
   #:gather-result
   #:previous
   #:gatherer
   #:catenate
   #:choose
   #:next-in
   #:mingle
   #:make-series
   #:*last-series-loop*
   #:iterate
   #:collect-ignore
   #:*series-expression-cache*
   #:result-of
   #:split
   #:positions
   #:spread
   #:scan-symbols
   #:*last-series-error*
   #:scan-lists-of-lists-fringe
   #:expand
   #:scan-plist
   #:split-if
   #:until-if
   #:fgather-result
   #:scan-fn-inclusive
   #:collect-stream
   #:next-out
   #:propagate-alterability
   #:collect-sum
   #:scan-hash
   #:collect-max
   #:subseries
   #:terminate-producing
   #:collect-product
   #:map-fn
   #:collect-min
   #:mapping
   #:latch
   #:until
   #:fgather-next
   #:scan-fn
   #:cotruncate
   #:alter
   #:choose-if
   #:scan-file
   #:scan-lists-of-lists
   #:scan
   #:*suppress-series-warnings*
   #:mask
   #:chunk
   #:Zdefun)
  (:export
   ;; utils
   #:in-syntax
   #:openi
   #:openo
   #:opena
   #:fstring
   #:fpathname))


(cl:in-package #:bcl)


(defmacro ^ ((&rest args) &body body)
  `(function (lambda (,@args) ,@body)))


(defmacro fun ((&rest args) &body body)
  `(function (lambda (,@args) ,@body)))


(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))


(eval-always 
  (setf (fdefinition 'a)
        (fdefinition 'make-instance))
  (setf (fdefinition 'prop)
        (fdefinition 'getf))
  (setf (fdefinition 'isa)
        (fdefinition 'typep))
  (setf (fdefinition 'call)
        (fdefinition 'funcall))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (loop :for s :being :the :external-symbols :of :cl
        :for sym := (intern (concatenate 'string "<" (string s) ">") :bcl)
        :when (find-class s nil)
        :do (eval (print `(defconstant ,sym (find-class ',s))))
        :and :collect (copy-symbol sym))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (loop :for s :being :the external-symbols :of :ppcre
        :do (when (and (fboundp s)
                       (not (search #.(string '#:regex) (string s))))
              (let ((name (intern (concatenate 'string
                                               (string "RE.")
                                               (string s))
                                  :bcl)))
                (if (macro-function s)
                    (setf (macro-function name)
                          (macro-function s))
                    (setf (fdefinition name)
                          (fdefinition s)))))
        :finally (progn
                   (setf (fdefinition 're.apropos-list)
                         (fdefinition 'cl-ppcre:regex-apropos-list))
                   (setf (fdefinition 're.apropos)
                         (fdefinition 'cl-ppcre:regex-apropos))
                   (setf (fdefinition 're.replace)
                         (fdefinition 'cl-ppcre:regex-replace))
                   (setf (fdefinition 're.replace-all)
                         (fdefinition 'cl-ppcre:regex-replace-all))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  )


(declaim (inline fstring))


(defun fstring (control &rest args)
  (apply #'format nil control args))


(defun fpathname (control &rest args)
  (pathname (apply #'fstring control args)))


(defun openi (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file :direction :input
        :element-type element-type
        :external-format external-format))


(defun openo (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file
        :direction :output
        :element-type element-type
        :external-format external-format
        :if-exists :supersede
        :if-does-not-exist :create))


(defun opena (file &key (element-type 'cl:character) (external-format :utf-8))
  (open file
        :direction :output
        :element-type element-type
        :external-format external-format
        :if-exists :append
        :if-does-not-exist :create))


(defmacro in-syntax (readtable)
  `(eval-always
     (setq *readtable* ,readtable)))


(eval-always
 (defun read-raw-string (srm chr arg)
   (declare (ignore chr arg))
   (with-output-to-string (out)
     (loop 
      (let ((c (read-char srm T nil T)))
        (if (eql #\" c)
            (if (eql #\" (peek-char nil srm T nil T))
                (progn
                  (write-char #\" out)
                  (read-char srm T nil T))
                (return))
            (write-char c out))))))


 (defun concat-string (srm chr arg)
   (declare (ignore arg))
   (with-output-to-string (out)
     (unread-char chr srm)
     (loop 
      (let ((s (read srm T nil T)))
        (write-string s out)
        (unless (eql #\" (peek-char T srm T nil T))
          (return))))))


 (defvar *bcl* (copy-readtable nil))


 (let ((*readtable* *bcl*))
   (make-dispatch-macro-character #\! T)
   (set-dispatch-macro-character #\! #\" #'read-raw-string)
   (set-dispatch-macro-character #\# #\" #'concat-string)
   (set-dispatch-macro-character
    #\! #\(
    (lambda (srm chr arg)
      (declare (ignore chr arg))
      (cons 'cl:funcall (read-delimited-list #\) srm T))))
   (set-dispatch-macro-character #\# #\Z
                                 #'series::series-reader)
   (set-dispatch-macro-character #\# #\M
                                 #'series::abbreviated-map-fn-reader)
   (set-dispatch-macro-character
    #\# #\@
    (lambda (srm chr arg)
      (declare (ignore chr arg))
      `(eval-always
        ,(read srm T nil T))))
   (set-dispatch-macro-character
    #\# (character "")
    (lambda (srm chr arg)
      (declare (ignore chr arg))
      (cons 'eval-always
            (read-delimited-list (character "") srm T)))))
            
 (defconstant bcl-syntax *bcl*))


(defmacro then (&body body)
  `(progn . ,body))


(defmacro else (&body body)
  `(progn . ,body))


(defmacro for (&rest body)
  `(loop 
    ,@(reduce (lambda (res b)
                (append res (->loop-clause b)))
              body
              :initial-value nil)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ->loop-clause (xpr)
    (case (find (car xpr) '(let) :test #'string-equal)
      (let (destructuring-bind (let &rest args)
                               xpr
             (declare (ignore let))
             `(for ,@args)))
      (otherwise xpr))))

(defmacro Zdefun (name (&rest args) &body body)
  `(series::defun ,name (,@args)
     (declare (series:optimizable-series-function))
     ,@body))


(eval-always
  (setf (fdefinition 'bcl::keep-if)
        (fdefinition 'cl:remove-if-not)))


(defgeneric bcl::name (obj)
  (:method ((obj cell-error)) (cell-error-name obj))
  (:method ((obj character)) (char-name obj))
  (:method ((obj class)) (class-name obj))
  (:method ((obj generic-function)) (generic-function-name obj))
  (:method ((obj package)) (package-name obj))
  (:method ((obj restart)) (restart-name obj))
  (:method ((obj slot-definition)) (slot-definition-name obj))
  (:method ((obj symbol)) (symbol-name obj)))



;;; *EOF*
