// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/55410

protocol P {}
typealias T = (P) -> Void
let x: T! = [1, 2, 3].reversed().reduce()
// expected-error@-1 {{no exact matches in call to instance method 'reduce'}}
// expected-note@-2 {{found candidate with type '(Optional<T>, (Optional<T>, Int) throws(_) -> Optional<T>) throws(_) -> Optional<T>' (aka '(Optional<(any P) -> ()>, (Optional<(any P) -> ()>, Int) throws(_) -> Optional<(any P) -> ()>) throws(_) -> Optional<(any P) -> ()>')}}
// expected-note@-3 {{found candidate with type '(__owned Optional<T>, (inout Optional<T>, Int) throws(_) -> ()) throws(_) -> Optional<T>' (aka '(__owned Optional<(any P) -> ()>, (inout Optional<(any P) -> ()>, Int) throws(_) -> ()) throws(_) -> Optional<(any P) -> ()>')}}
