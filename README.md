
# ShinyLisp

*The Lisp designed for golfing*

This is a work-in-progress golfing language with a Lisp-like syntax. Now, you might (rightfully) ask how Lisp can possibly be used as a golfing language with all those parentheses. To that, I present you with *chains*:

    (func argx argy)
    is equivalent to
    (funcArgxArgy)
    or even better
    FuncArgxArgy

If you have a chain of identifiers or literals, it will be split off at any capital letters so that `abCdEf` becomes `ab cd ef`. Further, if a chain starts with a capital letter, it will start its own sublist, so `AbCdEf` becomes `(ab cd ef)`. As a demonstration of how powerful this can be, the following are equivalent programs which create a function to compute the nth term of the Fibonacci sequence.

    (= fib [cond (== x 0) 0 (== x 1) 1 (p (fib (m x 1)) (fib (m x 2)))])
    +F[i +=X0 0 +=X1 1(p F:MX1 F:MX2)]
    +F'I:C,X2~X:P(f:MX1):F:MX2

Please see the wiki for details and a guide to getting started with the language.
