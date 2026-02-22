// RUN: %target-typecheck-verify-swift

protocol P {}
protocol Q {}
class C1 {}


// Valid: Protocol compositions used in constraints
func acceptGeneric<T: P & Q>(_: T) {}


// Valid: Existential composition as a function parameter
func acceptExistential(_: any P & Q) {}


// Valid: Class inheritance + protocol conformance (normal case)
class D: C1, P {}


// Valid: Protocol composition used in a typealias
typealias Both = P & Q
func useTypealias(_: Both) {}


// Expected to pass with no errors
