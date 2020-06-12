// RUN: %target-typecheck-verify-swift

protocol P {}
typealias T = (P) -> Void
let x: T! = [1, 2, 3].reversed().reduce()
// expected-error@-1 {{no exact matches in call to instance method 'reduce'}}
// expected-note@-2 2{{candidate has partially matching parameter list}}
