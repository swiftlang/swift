// RUN: %target-typecheck-verify-swift

protocol P1 : class { }
// expected-warning@-1 {{using 'class' keyword to define a class-constrained protocol is deprecated; use 'AnyObject' instead}} {{15-20=AnyObject}}

protocol P2 : class, class { } // expected-error{{redundant 'class' requirement}}{{20-27=}}
// expected-warning@-1 {{using 'class' keyword to define a class-constrained protocol is deprecated; use 'AnyObject' instead}} {{15-20=AnyObject}}

protocol P3 : P2, class { } // expected-error{{'class' must come first in the requirement list}}{{15-15=class, }}{{17-24=}}
// expected-warning@-1 {{using 'class' keyword to define a class-constrained protocol is deprecated; use 'AnyObject' instead}} {{19-24=AnyObject}}

struct X : class { } // expected-error{{'class' constraint can only appear on protocol declarations}}
