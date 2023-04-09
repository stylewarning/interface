# INTERFACE

By Robert Smith

## Introduction

This system contains an implementation of interfaces and
implementations. They're sometimes called protocols in other
languages.

Broadly speaking, an "interface" is some collection of function
"prototypes" that a valid implementation must implement. For example,
something called a "stack" must implement stack creation, pushing,
peeking, and popping.

The notion of interfaces and implementations aid the distinction
between data structures and different implementations of that data
structure. This was perhaps pioneered by Modula-3, and became a
significant part of other languages like Standard ML and OCaml. In all
of the aforementioned languages, interfaces can actually contain more
than just functions, such as types and values. Haskell typeclasses are
also a form of interface and implementation. They are very general and
are even parametric.

One way to accomplish the notion of interfaces and implementations in
Lisp is to use some "abstract class" and make several (final)
subclasses of that class. The interface, in this case, is the abstract
class and a collection of generic functions. The implementation would
be the final subclass along with method definitions.

For example:

```lisp
(defclass stack () ())
(defgeneric make-stack (impl))
(defgeneric stack-push (impl s x))
(defgeneric stack-pop (impl s))
(defgeneric stack-peek (impl s))

(defclass list-stack (stack) ())
(defmethod make-stack ((impl list-stack))
  nil)
(defmethod stack-push ((impl list-stack) s x)
  (cons x s))
(defmethod stack-pop ((impl list-stack) s)
  (cdr s))
(defmethod stack-peek ((impl list-stack) s)
  (car s))
```

This is mostly sufficient, though Lisp makes no guarantee that a class
will have any set of methods defined for it. (One could perhaps use
the MOP for this.) One can "optimize" implementations by conflating
the notion of an implementation with the actual data structure being
implemented, and make it a part of the implementation class. In this
case, we could have a slot in `LIST-STACK` holding the list.

Since methods are not tied to classes, this implementation allows one
to have a class implement several methods. Also, it is entirely
possible to do away with the superclass; that is a formality tying all
implementations to a particular interface with a name.

As I understand, this basic notion is taken to the extreme with Fare's
[Lisp Interface Library](http://www.cliki.net/lisp-interface-library).

In this system, however, we take a different approach
entirely. Instead of using a class to represent interfaces and
implementations, we have a structure whose slots are the
implementation functions. The name of the structure (which decides
what slots it has) is the interface, and the implementation is the
actual slot values.

It is cumbersome, however, to use an interface by accessing slots all
of the time. Instead, we define functions---which correspond to the
slot names---which access the slots of an implementation and pass the
arguments to it.

In doing this, there's no dispatch on type required, just access on
the slots of the structure. It also forces data structures and the
interface to be completely disjoint entities.


## Example

```lisp
(define-interface stack ()
  (make-stack (&rest r))
  (push-stack (s x))
  (peek-stack (s))
  (pop-stack (s)))

(define-implementation list-stack (stack)
  :make-stack
  (lambda (&rest r)
    r)

  :push-stack
  (lambda (s x)
    (cons x s))

  :peek-stack
  (lambda (s)
    (car s))
  
  :pop-stack
  (lambda (s)
    (cdr s)))

(define-implementation vector-stack (stack)
  :make-stack
  (lambda (&rest r)
    (let ((length (length r)))
      (make-array length
                  :adjustable t
                  :fill-pointer length
                  :initial-contents r)))
  
  :push-stack
  (lambda (s x)
    (vector-push-extend x s)
    s)
  
  :peek-stack
  (lambda (s)
    (aref s (1- (length s))))
  
  :pop-stack
  (lambda (s)
    (vector-pop s)
    s))

;;; CL-USER> (pop-stack vector-stack
;;;                     (push-stack vector-stack
;;;                                 (make-stack vector-stack 1 2 3)
;;;                                 5))
;;; #(1 2 3)
;;; CL-USER> (pop-stack list-stack
;;;                     (push-stack list-stack
;;;                                 (make-stack list-stack 1 2 3)
;;;                                 5))
;;; (1 2 3)
```

## Performance

This implementation has been measured to be between 10% and 30% faster
than the classes approach described above. See the file
`interface-bench.lisp`.

## Other notes

The package also has a handy utility function called
`CALLING-FORM`. It solves the following problem:

Consider a function `F` with a lambda list `(L...)`. How can we write
a function `G`

```lisp
(defun G (L...)
  <???>)
```

such that calls to `G` are precisely equivalent to `F`? We can use

```lisp
(calling-form 'f '(L...))
```

which will produce code which is suitable for the definition of `G`.
