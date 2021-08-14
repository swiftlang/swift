// RUN: %target-typecheck-verify-swift 

protocol P: class {}
protocol P1: AnyObject {}
protocol P2 {}
protocol P3: class, P2 {} 
protocol P4: P2, class {} // expected-error {{'class' must come first in the requirement list}}
