# Delayed Evaluation

The evaluation strategy in Scheme is called applicative order evaluation.
Scheme first evalutes the operands (producing arguments) and the operator
(producing a procedure) and then applies the procedure to the arguments.
We say that the arguments are passed to the procedure "by value."
In this case study, we will explore the idea of delayed evaluation of
certain expressions.

Scheme has two primitives: `delay` takes an expression, "freezes" it, and
returns a "promise"; `force` redeems a promise by "thawing" it, evaluate
the expression, and returns the result. `(force (delay expr))` performs
a normal evaluation.

```
(delay expr) => promise
(force promise) => value
```

```scheme
(define-syntax my-delay
  (syntax-rules ()
    ((my-delay expr)
     (lambda ()
        expr))))

(define my-force
  (lambda (x) (x)))

(define d (my-delay (/ 1 0)))

d
; => #<procedure>

(my-force d)
; /: division by zero
```

Our macro `my-delay` takes an expression and expands to a "thunk" that
evaluates to the expression. A "thunk" is a zero-argument function and
because its body is not evaluated till the "thunk" is invoked it can
be used to delay the evaluation of an expression, e.g. `(define thunk (lambda () (+ 3 4)))`.

The special form (macro) `delay` create a promise. When the promise
is forced for the first time, the value of the delayed/postponed
expression is computed and returned. Each succeeding time the promise
is forced, the same value that was computed the first tie is
returned as shown in the following example:
```scheme
(define d (delay (random 10)))

d
;=> #<promise>

(force d)
;=> 7

(force d)
;=>7
```

We can mimic this behavior in our `my-dealy` as follows:

```scheme
(define-syntax my-delay
  (syntax-rules ()
    ((my-delay expr)
     (make-promise
      (lambda ()
        expr)))))

(define my-force
  (lambda (x) (x)))

(define make-promise
  (lambda (thunk)
    (let ((already-run? #f)
          (result "any value"))
      (lambda ()
        (if (not already-run?)
            (begin
              (set! result (my-force thunk))
              (set! already-run? #t)
              result)
            result)))))

(define d (my-delay (random 10)))

d
;=> #<procedure>

(my-force d)
;=> 8

(my-force d)
;=> 8

(delay (+ 5 6))
; => #<promise>

(force 42)
; => 42

(let ((delayed (delay (+ 5 6))))
  (force delayed))
; => 11
```
## Delayed list

`delay` and `force` allow us to postpone evaluation of an expression
till it is needed. Creating a promise has the effect of "memoizing"
the body as well as delaying its evaluation. We can use this
"lazy evaluation to handle infinite lists.
```scheme
(define (fib n)
  (printf "fib(~s)\n" n)
  (if (or (= n 1) (= n 2))
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

;(trace fib)

(define del-list
  (cons (fib 8)
        (delay (cons (fib 9)
                     (delay '())))))

(define delayed-list-car
  (lambda (x)
    (car x)))

; (define delayed-list-car car)

;(delayed-list-car del-list)
;
;(cdr del-list)
;
;(force (cdr del-list))
;
;(car (force (cdr del-list)))

(define delayed-list-cdr
  (lambda (x)
    (force (cdr x))))

#;(define delayed-list-cdr
  (compose force cdr))

;(car (delayed-list-cdr del-list))
;
;(car (delayed-list-cdr del-list))
;
;(car del-list)


(define delayed-list-null?
  (lambda (delayed-list)
    (null? delayed-list)))

; (define delayed-list-null? null?)

(define the-null-delayed-list '())

(define-syntax delayed-list-cons
  (syntax-rules ()
    ((delayed-list-cons x y)
     (cons x (delay y)))))

(define del-list1
  (delayed-list-cons
   (fib 8)
   (delayed-list-cons
    (fib 9)
    the-null-delayed-list)))

(define random-delayed-list
  (lambda (n)
    (if (zero? n)
        the-null-delayed-list
        (delayed-list-cons
         (+ 2 (random 11))
         (random-delayed-list (sub1 n))))))

(define sum-until-first-7
  (letrec
      ((local-sum
        (lambda (delayed-list sum count)
          (if (delayed-list-null? delayed-list)
              (printf
               "A seven was not found; sum = ~s  and count = ~s\n"
               sum count)
              (let ((next (delayed-list-car delayed-list)))
                (if (= next 7)
                    (printf "sum = ~s when count = ~s\n" sum count)
                    (local-sum
                     (delayed-list-cdr delayed-list)
                     (+ next sum)
                     (add1 count))))))))
    (lambda (rand-delayed-list)
      (local-sum rand-delayed-list 0 0))))

(sum-until-first-7 (random-delayed-list 100))

(sum-until-first-7 (random-delayed-list 100))

(define delayed-list->list
  (lambda (delayed-list)
    (if (delayed-list-null? delayed-list)
        '()
        (cons (delayed-list-car delayed-list)
              (delayed-list->list (delayed-list-cdr delayed-list))))))

(delayed-list->list (random-delayed-list 20))
```

## Stream
A stream is an infinite list. We can work with lists of infinite length
by delaying the evaluation of every list tail using delay. As an invariant,
every list tail will be delayed.

First, we need to define a vocabulary of stream functions.
```scheme
(cons-stream a b)  ; ~   (cons a (delay b))

(define head car)

(define (tail stream) (force (cdr stream)))

(define empty-stream? null?)

(define the-empty-stream '())
```

We also need to define `cons-stream` as a macro because we are not allowed to evaluate the second parameter.
```scheme
; (cons-stream a b)  means  (cons a (delay b))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream x y)
     (cons x (delay y)))))
```

We can define a a infinite list of ones:
```scheme
(define ones (cons-stream 1 ones))
```
We can use the function `stream-section` to extract a certain prefix of the list (determined by the first parameter of `stream-section`):
```scheme
(define (stream-section n stream)
  (cond ((= n 0) '())
        (else
          (cons
            (head stream)
            (stream-section
             (- n 1)
             (tail stream))))))

(stream-section 7 ones)
; => '(1 1 1 1 1 1 1)
```

We can define a stream of all natural numbers, using the function `integers-starting-from`:
```scheme
(define (integers-starting-from n)
 (cons-stream n
  (integers-starting-from (+ n 1))))

(define natural-nums
  (integers-starting-from 1))

(stream-section 10 natural-nums)
; => '(1 2 3 4 5 6 7 8 9 10)
```

Alternatively, we could use `add-streams` to produce natural numbers:
```scheme
(define (add-streams s1 s2)
 (let ((h1 (head s1))
       (h2 (head s2)))
   (cons-stream
     (+ h1 h2)
     (add-streams (tail s1) (tail s2)))))

(define natural-nums1
 (cons-stream 1
  (add-streams ones natural-nums1)))

(stream-section 10 natural-nums1)
; => '(1 2 3 4 5 6 7 8 9 10)
```

We can define `fibs` by adding `fibs` to its own tail:
```scheme
(define fibs
  (cons-stream 0
    (cons-stream 1
      (add-streams (tail fibs) fibs))))

(stream-section 15 fibs)
; => '(0 1 1 2 3 5 8 13 21 34 55 89 144
 233 377)
```

The [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes)
is a more sophisticated example of the use of streams:

```scheme
(define (sieve stream)
   (cons-stream
     (head stream)
     (sieve
       (filter-stream
         (lambda (x) (not (divisible? x (head stream))))
         (tail stream)))))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (filter-stream p lst)
  (cond ((empty-stream? lst) the-empty-stream)
        ((p (head lst)) (cons-stream (head lst) (filter-stream p (tail lst))))
        (else (filter-stream p (tail lst)))))

(define primes (sieve (integers-starting-from 2)))

(stream-section 50 primes)
; => '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229)
```

Notice that the prime numbers are really generated on demand. In the call `(stream-section 50 primes)`
we are requesting 50 prime numbers. This triggers generation of sufficient natural numbers via
`(integers-starting-from 2)`, and it triggers the set up of sufficient sieves to produce the result.

## File I/O
```scheme
(define port-in (open-input-file "input.dat"))

(read port-in)

(read port-in)

(read port-in)

(read port-in)

(read port-in)

(close-input-port port-in)

(let ((p (open-input-file "input2.dat")))
  (letrec
      ((add-items (lambda (sum)
                    (let ((item (read p)))
                      (cond
                        ((eof-object? item)
                         (close-input-port p)
                         sum)
                        (else (add-items (+ item sum))))))))
    (add-items 0)))


(let ((p (open-input-file "input3.dat")))
  (letrec
      ((reader (lambda (ch)
                 (if (eof-object? ch)
                     '()
                     (cons ch (reader (read-char p)))))))
    (let ((ans (reader (read-char p))))
      (close-input-port p)
      ans)))
```
input.dat:
```
This is "a test string."
((1 2) (3 4))
```
input2.dat:
```
100
150
200
250
```
input3.dat:
```
Testing 1 2 3
```

## Input and ouput as streams
```scheme
(define stream-car car)

(define stream-cdr (compose force cdr))

(define random-stream-generator
  (lambda ()
    (stream-cons
     (+ 2 (random 11))
     (random-stream-generator))))

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons x y)
     (cons x (delay y)))))

(define the-end-of-stream-tag "end of stream")

(define the-null-stream
  (stream-cons the-end-of-stream-tag the-null-stream))

(define list->stream
  (lambda (ls)
    (if (null? ls)
        the-null-stream
        (stream-cons (car ls)
                     (list->stream (cdr ls))))))

(define end-of-stream?
  (lambda (x)
    (eq? x the-end-of-stream-tag)))

(list->stream (let ((x (list 1 2 3))) (append x x)))

(define stream-null? (compose end-of-stream? stream-car))

(define stream->list
  (lambda (strm n)
    (if (or (zero? n) (stream-null? strm))
        '()
        (cons (stream-car strm)
              (stream->list (stream-cdr strm) (sub1 n))))))

(define finite-stream->list
  (lambda (finite-strm)
    (stream->list finite-strm -1)))

(define file->stream
  (lambda (filename)
    (let ((port-in (open-input-file filename)))
      (letrec
          ((build-input-stream
            (lambda ()
              (let ((ch (read-char port-in)))
                (if (eof-object? ch)
                    (begin
                      (close-input-port port-in)
                      the-null-stream)
                    (stream-cons ch (build-input-stream)))))))
        (build-input-stream)))))

(file->stream "input.dat")

;(stream->list (file->stream "input.dat") -1)

(require srfi/1)
(stream->list (list->stream (circular-list 1 2 3)) 10)
```


Sources:
* Lecture notes from https://homes.cs.aau.dk/~normark/prog3-03/html/notes/eval-order_themes-delay-stream-section.html
* Chapter 14 of "Scheme and the Art of Programming" https://www.cs.unm.edu/~williams/cs357/springer-friedman.pdf
* Chapter 8 of "The Scheme Programming Language" https://www.scheme.com/tspl4/syntax.html#./syntax:h0
* Racket documentation https://docs.racket-lang.org/reference/Delayed_Evaluation.html
