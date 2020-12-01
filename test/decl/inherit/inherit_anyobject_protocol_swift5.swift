// RUN: %target-typecheck-verify-swift -swift-version 5

protocol P: class {}
// expected-warning@-1 {{using 'class' keyword for protocol inheritance is deprecated; use 'AnyObject' instead}} {{13-18=AnyObject}} 
protocol P1: AnyObject {}
protocol P2 {}
protocol P3: class, P2 {} 
// expected-warning@-1 {{using 'class' keyword for protocol inheritance is deprecated; use 'AnyObject' instead}} {{14-19=AnyObject}}
protocol P4: P2, class {} // expected-error {{'class' must come first in the requirement list}}
// expected-warning@-1 {{using 'class' keyword for protocol inheritance is deprecated; use 'AnyObject' instead}} {{18-23=AnyObject}}
