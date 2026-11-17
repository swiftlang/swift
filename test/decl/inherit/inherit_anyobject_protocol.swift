// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift%language_mode-
// RUN: %target-typecheck-verify-swift -language-mode 4 -verify-additional-prefix swift4-

protocol P: class {}
// expected-swift5-warning@-1 {{using 'class' keyword to define a class-constrained protocol is deprecated; use 'AnyObject' instead}} {{13-18=AnyObject}}
protocol P1: AnyObject {}
protocol P2 {}
protocol P3: class, P2 {} 
// expected-swift5-warning@-1 {{using 'class' keyword to define a class-constrained protocol is deprecated; use 'AnyObject' instead}} {{14-19=AnyObject}}
protocol P4: P2, class {} // expected-error {{'class' must come first in the requirement list}}
// expected-swift5-warning@-1 {{using 'class' keyword to define a class-constrained protocol is deprecated; use 'AnyObject' instead}} {{18-23=AnyObject}}
