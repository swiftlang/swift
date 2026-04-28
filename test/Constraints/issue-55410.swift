// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

// https://github.com/apple/swift/issues/55410

protocol P {}
typealias T = (P) -> Void
let x: T! = [1, 2, 3].reversed().reduce()
// expected-error@-1 {{no exact matches in call to instance method 'reduce'}}
// expected-note@-2 {{found candidate with type '(consuming T?, (consuming T?, Int) throws(E) -> T?) throws(E) -> T?' (aka '(consuming Optional<(any P) -> ()>, (consuming Optional<(any P) -> ()>, Int) throws(E) -> Optional<(any P) -> ()>) throws(E) -> Optional<(any P) -> ()>')}}
// expected-note@-3 {{found candidate with type '(__owned T?, (inout T?, Int) throws(E) -> ()) throws(E) -> T?' (aka '(__owned Optional<(any P) -> ()>, (inout Optional<(any P) -> ()>, Int) throws(E) -> ()) throws(E) -> Optional<(any P) -> ()>')}}
