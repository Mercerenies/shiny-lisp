
# ShinyLisp

*The Lisp designed for golfing*

This is a work-in-progress golfing language with a Lisp-like syntax. Now, you might (rightfully) ask how Lisp can possibly be used as a golfing language with all those parentheses. To that, I present you with *chains*:

    (func argx argy)
    is equivalent to
    (funcArgxArgy)
    or, even better
    FuncArgxArgy

If you have a chain of identifiers or literals, it will be split off at any capital letters so that `abCdEf` becomes `ab cd ef`. Further, if a chain starts with a capital letter, it will start its own sublist, so `AbCdEf` becomes `(ab cd ef)`.

This language is a work-in-progress. I will write up a wiki when it gets a bit further along.
