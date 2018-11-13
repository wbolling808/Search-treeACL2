;;; The following is boilerplate to get the testing framework working
;;; with the new version of ProofPad

(encapsulate nil
  (set-state-ok t)
  (program)
  (defun check-expect-fn (left right sexpr state)
    (if (equal left right)
      (mv nil (cw "check-expect succeeded: ~x0~%" sexpr) state)
      (er soft nil "check-expect failed: ~x0
      Expected: ~x1
      Actual:   ~x2" sexpr right left)))
  (defmacro check-expect (&whole sexpr left right)
    `(check-expect-fn ,left ,right (quote ,sexpr) state))
  (logic))

;(add-include-book-dir :teachpacks "/Applications/Proof\ Pad.app/Contents/Resources/Java/acl2/dracula/")
(include-book "doublecheck" :uncertified-okp t :dir :teachpacks)

(include-book "misc/total-order" :dir :system)

;;; Note: We use << instead of < because << works for
;;; symbols (like 'x and 'y), while < only works for
;;; numbers

;;; Also, you should know that (symbolp x) is true if
;;; x is a valid symbol, and (rationalp x) is true if
;;; x is a rational number.  These two predicates are
;;; just like consp (for lists) and natp (for naturals)

;;; This function returns the maximum of its two inputs
(defun max-<< (a b)
   (if (<< a b)
       b
       a))

;;; TODO: Write more unit tests for max-<<, at least 5 total.
(check-expect (max-<< 'a 'b) 'b)
(check-expect (max-<< 'c 'b) 'c)

;;; This function returns the minimum of its two inputs
(defun min-<< (a b)
   (if (<< a b)
       a
       b))

;;; TODO: Write more unit tests for min-<<, at least 5 total.
(check-expect (min-<< 'a 'b) 'a)
(check-expect (min-<< 'c 'b) 'b)

;;; Trees look like lists as in class:
;;; A tree can be:
;;;    1. NIL                    - the empty tree
;;;    2. (key value left right) - where left and right are trees

;;; This function finds the maximum key in a tree
(defun tree-max (tree)
   (if (consp tree)
       (if (consp (third tree))
           (if (consp (fourth tree))
               (max-<< (first tree)
                    (max-<< (tree-max (third tree)) 
                            (tree-max (fourth tree))))
               (max-<< (first tree)
                       (tree-max (third tree))))
           (if (consp (fourth tree))
               (max-<< (first tree)
                       (tree-max (fourth tree)))
               (first tree)))
       'MIN-SYMBOL))

;;; TODO: Write more unit tests for tree-max, at least 5 total.
(check-expect (tree-max '(x 1 nil (z 3 (y 2 nil nil) nil))) 'z)
(check-expect (tree-max '(x z nil (w 3 (y 2 nil nil) nil))) 'z)
(check-expect (tree-max '(y 1 nil (3 3 (y 2 z nil) nil))) 'z)
(check-expect (tree-max '(x 1 nil (2 3 (y 2 nil z) nil))) 'z)
(check-expect (tree-max '(x 1 nil (5 3 (y 2 nil 26) nil))) '26)

;;; This function finds the minimum key in a tree
(defun tree-min (tree)
   (if (consp tree)
       (if (consp (third tree))
           (if (consp (fourth tree))
               (min-<< (first tree)
                       (min-<< (tree-min (third tree)) 
                               (tree-min (fourth tree))))
               (min-<< (first tree)
                       (tree-min (third tree))))
           (if (consp (fourth tree))
               (min-<< (first tree)
                       (tree-min (fourth tree)))
               (first tree)))
       'MAX-SYMBOL)
   )

;;; TODO: Write more unit tests for tree-min, at least 5 total.
(check-expect (tree-min '(x 1 nil (z 3 (y 2 nil nil) nil))) 'x)
(check-expect (tree-min '(x z nil (w 3 (y 2 nil nil) nil))) 'x)
(check-expect (tree-min '(y 1 nil (3 3 (y 2 z nil) nil))) 'y)
(check-expect (tree-min '(1 1 nil (2 3 (y 2 nil z) nil))) '1)
(check-expect (tree-min '(t 1 nil (5 3 (y 2 nil 26) nil))) 't)
;;; This function checks whether the tree is a valid search tree
;;; The conditions are that:
;;;   1.   The tree is NIL, OR
;;;   2.1. The tree is a list with 4 elements (key value left right), AND
;;;   2.2. The key is a valid symbol, AND
;;;   2.3. The value is a rational number, AND
;;;   2.4. The left tree is NIL, OR the max of the left tree is << the key, AND
;;;   2.5. The right tree is NIL, OR the key is << the min of the right, AND
;;;   2.6. The left tree is a valid search tree, AND
;;;   2.7. The right tree is a valid search tree
;;; TODO: Implement this function using the strategy above.
(defun search-treep (tree)
   (if (consp tree)
      (and (if (= (len tree) 4) ;;; 2.1 through 2.7
	   (and  (symbolp (first tree))
                 (rationalp (second tree))
				(if ( and (or (= (third tree) nil) (<< (max (third tree)) (first tree))) 
						(and (or ( = (fourth tree) nil) (<< (min (fourth tree)) (first tree))
						;2.6 & 2.7 
       t   ;;; 1.
       ))

;;; TODO: More unit tests, at least 5 total
(check-expect (search-treep '(x 1 nil (z 3 (y 2 nil nil) nil))) t)
(check-expect (search-treep '(y 1 nil (z 3 (x 2 nil nil) nil))) nil)

;;; TODO: Implement insertion of the key/value pair into the search tree
;;; Hint: The equations for tree-insert were covered in class
(defun tree-insert (key value tree)
  	(if (consp tree) ;if the tree is not empty
 		 (if (= key (first tree))   ;check if the first value in tree is the key
  		 (cons a '(key (third tree) (fourth tree)))  ;If the first value is the key construct a list [key , value, Third(tree), Fourth(tree)
				(if (< key (first tree))    ; if key is not = first key, but if key is less than first in tree 
				  (cons (first tree) '((second tree) (cons a key) (third tree) (fourth tree)))) ;create tree first second (insert key value third fourth)
					(if(> key (first tree)); if it is on the other side of the tree
					cons (first tree) '((second tree)  (third tree)(cons a '(key (fourth tree))))  ;create tree first second third (insert key value fourth))
					;third case for if?
					)
			)
 (cons key value)) ;Return cons key value if tree is nil	
  )

;;; TODO: More unit tests, at least 5 total
(check-expect (tree-insert 'x 1 nil) '(x 1 nil nil))
(check-expect (tree-insert 'z 3 '(x 1 nil nil)) '(x 1 nil (z 3 nil nil)))
(check-expect (tree-insert 'y 2 '(x 1 nil (z 3 nil nil))) '(x 1 nil (z 3 (y 2 nil nil) nil)))

;;; TODO: Create a tree by inserting the key/value pairs in a list
;;; To make life simpler, insert the first key/value pair last into the tree
;;; Trust me: that makes the recursion simpler.
;;; Look at the unit tests below to see how this should work
(defun search-list-to-tree (l)
  (if (endp l)
      (consp l)
    (tree-insert (car l) (search-list-to-tree (cdr l))))); car returns first pair in a list and cdr returns the second

;;; TODO: More unit tests, at least 5 total
(check-expect (search-list-to-tree '((x 1) (y 2))) 
              '(y 2 (x 1 nil nil) nil))
(check-expect (search-list-to-tree '((b 1) (c 2) (a 3))) 
              '(a 3 nil (c 2 (b 1 nil nil) nil)))

(defun tree-lookup (key tree)
  (if (consp tree)
    (if (equal key (first tree))
      (second tree)
      (if (<< key (first tree))
        (tree-lookup key (third tree))
        (tree-lookup key (fourth tree))))
    0))

(check-expect (tree-lookup 'x '(x 1 nil nil)) 1)
(check-expect (tree-lookup 'y '(x 1 nil nil)) 0)
(check-expect (tree-lookup 'y '(x 1 nil (z 3 (y 2 nil nil) nil))) 2)

;;; Before we get to the proofs, we have to extend
;;; the doublecheck library that defines defproperty. 
;;; That library has support for random numbers and 
;;; lists of random numbers, but it doesn't support 
;;; more complicated objects, like binary search trees
;;; or even trees in general.
;;;
;;; The extensions require deeper knowledge of ACL2 than
;;; I expect you to get from the brief introduction in
;;; this class, so I've written the extensions for you.
;;; You can ignore this code. You're welcome to look through
;;; it, of course, but you can simply skip until you see 
;;; the comment "END OF RANDOM TREES". After that, you can
;;; continue with the little theories and proofs to 
;;; complete this assignment.

(defun code-char-list (l)
   (if (consp l)
       (cons (code-char (first l))
             (code-char-list (rest l)))
       nil))
(check-expect (code-char-list '(65 66 67)) (list #\A #\B #\C))

(set-state-ok t)
(defun random-symbol-of-length-fn (n state)
   (mv-let (nums state)
           (random-between-list-fn 65 90 n state)
           (mv (intern (coerce (code-char-list nums) 'string) "ACL2") state)))
(defmacro random-symbol-of-length (n)
   `(random-symbol-of-length-fn ,n state))
(defmacro random-symbol ()
   `(mv-let (ln state)
            (random-data-size)
       (random-symbol-of-length-fn ln state)))

(defun random-search-list-of-length-fn (n state)
   (if (zp n)
       (mv nil state)
       (mv-let (l state)
               (random-search-list-of-length-fn (- n 1) state)
          (mv-let (keylen state)
                  (random-data-size)
             (mv-let (key state)
                     (random-symbol-of-length-fn keylen state)
                (mv-let (val state)
                        (random-natural)
                   (mv (cons (list key val) l) state)))))))

(defmacro random-search-list-of-length (n)
   `(random-search-list-of-length-fn ,n state))
(defmacro random-search-list ()
   `(mv-let (ln state)
            (random-data-size)
       (random-search-list-of-length-fn ln state)))

(in-theory (enable natp-random$ random$-linear mv-nth))

(defun random-natural-tree-of-length-fn (n state)
   (if (zp n)
       (mv nil state)
       (mv-let (n1 state)
               (random-between 0 (- n 1))
          (let ((n2 (- n (+ n1 1))))
               (mv-let (left state)
                       (random-natural-tree-of-length-fn n1 state)
                  (mv-let (right state)
                          (random-natural-tree-of-length-fn n2 state)
                     (mv-let (keylen state)
                             (random-data-size)
                        (mv-let (key state)
                                (random-symbol-of-length-fn keylen state)
                           (mv-let (val state)
                                   (random-natural)
                              (mv (list key val left right) state))))))))))
(defmacro random-natural-tree-of-length (n)
   `(random-natural-tree-of-length-fn ,n state))
(defmacro random-natural-tree ()
   `(mv-let (ln state)
            (random-data-size)
       (random-natural-tree-of-length-fn ln state)))

(defmacro random-search-tree-of-length (n)
   `(mv-let (l state)
            (random-search-list-of-length-fn ,n state)
       (mv (search-list-to-tree l)
           state)))

(defmacro random-search-tree ()
   `(mv-let (ln state)
            (random-data-size)
       (random-search-tree-of-length ln)))

;;; END OF RANDOM TREES

;;; Below is the last part of the assignment, so you should 
;;; continue reading carefully.

;;; TODO: Verify using defproperty-program the little theory that:
;;;    * The tree-min of a tree is << the tree-max of the tree
;;; Careful! This property is not true for all trees! It's
;;; only true for non-empty trees when the minimum is not equal
;;; to the maximum
(defproperty-program treemin-<<-treemax
   (tree  :value (random-search-tree))
   ...)

;;; TODO: Now convert the little theory above into a theorem
;;; with defproperty
(defproperty treemin-<<-treemax
   ...)

;;; I did this one for you -- dilly dilly!  This verifies that
;;; the treemin of the tree that results from inserting a
;;; key/value pair to an original tree is either
;;;    1. The new key
;;;    2. The treemin of the original tree
;;; depending on whether the new key is <<= the old treemin
;;; or not
(defproperty-program treemin-of-insert
   (tree  :value (random-search-tree)
    key   :value (random-symbol)
    value :value (random-natural))
   (equal (tree-min (tree-insert key value tree))
          (if (or (equal tree nil)
                  (<< key (tree-min tree)))
              key
              (tree-min tree))))

;;; This function does absolutely nothing, so don't even try
;;; to understand what it returns.  (I don't even know myself.)
;;; Its only purpose in life is to define an induction scheme
;;; (e.g., a special version of noetheric induction) that is
;;; needed to prove the next two theorems.
(defun iih (key tree)
  (if (consp tree)
    (if (equal key (first tree))
      key
      (if (<< key (first tree))
        (iih key (third tree))
        (iih key (fourth tree))))
    nil))

;;; I did this one for you, too. Dilly dilly! This converts the
;;; previous theorem about the treemin after inserting into
;;; a binary search tree
(defproperty-logic treemin-of-insert
   (tree  :value (random-search-tree)
    key   :value (random-symbol)
    value :value (random-natural))
   (implies (and (search-treep tree)
                 (symbolp key)
                 (rationalp value))
            (equal (tree-min (tree-insert key value tree))
                   (if (or (not (consp tree))
                           (and (consp tree)
                                (<< key (tree-min tree))))
                       key
                       (tree-min tree))))
   :hints (("Goal" :induct (iih key tree))
           ("Subgoal *1/3"
           :use ((:instance <<-trichotomy
                            (x key)
                            (y (first tree)))
                 (:instance <<-trichotomy
                            (x key)
                            (y (tree-min (third tree))))
                 (:instance <<-trichotomy
                            (x key)
                            (y (tree-min (fourth tree)))))
           :in-theory (disable <<-trichotomy))
           ("Subgoal *1/2"
           :use ((:instance <<-trichotomy
                            (x key)
                            (y (first tree)))
                 (:instance <<-trichotomy
                            (x key)
                            (y (tree-min (third tree))))
                 (:instance <<-trichotomy
                            (x key)
                            (y (tree-min (fourth tree))))))))

;;; TODO: Now you can do the property about the treemax
;;; after inserting into a binary tree.  This is almost
;;; identical to treemin-of-insert above, so take a look
;;; at that property, make sure you understand it, then
;;; copy/paste below and modify it so it's about treemax
;;; instead of treemin
(defproperty-program treemax-of-insert
   (tree  :value (random-search-tree)
    key   :value (random-symbol)
    value :value (random-natural))
   (implies (and (search-treep tree)
                 (symbolp key)
                 (rationalp value))
            (equal (tree-max (tree-insert key value tree))
                   (if (or (not (consp tree))
                           (and (consp tree)
                                (>> key (tree-max tree))))
                       key
                       (tree-max tree))))
   :hints (("Goal" :induct (iih key tree))
           ("Subgoal *1/3"
           :use ((:instance >>-trichotomy
                            (x key)
                            (y (first tree)))
                 (:instance >>-trichotomy
                            (x key)
                            (y (tree-max (third tree))))
                 (:instance >>-trichotomy
                            (x key)
                            (y (tree-max (fourth tree)))))
           :in-theory (disable >>-trichotomy))
           ("Subgoal *1/2"
           :use ((:instance >>-trichotomy
                            (x key)
                            (y (first tree)))
                 (:instance >>-trichotomy
                            (x key)
                            (y (tree-max (third tree))))
                 (:instance >>-trichotomy
                            (x key)
                            (y (tree-max (fourth tree))))))))


;;; TODO: And now convert that little theory into a
;;; theorem. Again, start with treemin-of-insert, make
;;; sure you understand it, then copy/paste/modify below.
(defproperty-logic treemax-of-insert
  (tree  :value (random-search-tree)
    key   :value (random-symbol)
    value :value (random-natural))
   (implies (and (search-treep tree)
                 (symbolp key)
                 (natlp value))
            (equal (tree-max (tree-insert key value tree))
                   (if (or (not (consp tree))
                           (and (consp tree)
                                (>> key (tree-max tree))))
                       key
                       (tree-max tree))))
   :hints (("Goal" :induct (iih key tree))
           ("Subgoal *1/3"
           :use ((:instance >>-trichotomy
                            (x key)
                            (y (first tree)))
                 (:instance >>-trichotomy
                            (x key)
                            (y (tree-max (third tree))))
                 (:instance >>-trichotomy
                            (x key)
                            (y (tree-max (fourth tree)))))
           :in-theory (disable >>-trichotomy))
           ("Subgoal *1/2"
           :use ((:instance >>-trichotomy
                            (x key)
                            (y (first tree)))
                 (:instance >>-trichotomy
                            (x key)
                            (y (tree-max (third tree))))
                 (:instance >>-trichotomy
                            (x key)
                            (y (tree-max (fourth tree))))))))

;;; TODO: Now verify using defproperty-program that
;;; the result of inserting a key/value pair into a search-tree
;;; is also a search-tree
(defproperty-program insert-on-search-tree-is-still-search-tree
  ...)

;;; TODO: Now convert that property into a theorem using
;;; defproperty
(defproperty insert-on-search-tree-is-still-search-tree
  ...)

;;; Now we will verify the basic properties of lookup
;;; and insert together. We will break this up into two
;;; cases:
;;;   1. We insert a key/value pair, then do a lookup
;;;      of the *same* key we just inserted
;;;   2. We insert a key/value pair, then do a lookup
;;;      of a *different* key than the one we just inserted
;;; It's easier to break this up into two cases, as
;;; follows.

;;; TODO: Now verify using defproperty-program the
;;; first case of the lookup/insert property above
(defproperty-program tree-lookup-insert-1
   ...)

;;; TODO: Now turn that property into a theorem using 
;;; defproperty
(defproperty-logic tree-lookup-insert-1
   ...)

;;; TODO: Now verify using defproperty-program the
;;; second case of the lookup/insert property above
(defproperty-program tree-lookup-insert-2
   ...)

;;; TODO: Finally turn that property into a theorem using 
;;; defproperty
(defproperty-logic tree-lookup-insert-2
   ...)
