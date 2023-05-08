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
    ((delay expr)
     (lambda ()
        expr))))

(define my-force
  (lambda (x) (x)))
```

## Examples
```scheme
(delay (+ 5 6))
; => #<promise>

(force 42)
; => error

(let ((delayed (delay (+ 5 6))))
  (force delayed))
; => 11
```

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

we also need to define `cons-stream` as a macro because we are not allowed to evaluate the second parameter.
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

Sources:
* https://homes.cs.aau.dk/~normark/prog3-03/html/notes/eval-order_themes-delay-stream-section.html
* Chapter 14 of "Scheme and the Art of Programming"
