// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/53382

protocol P {}
struct S<T> {}
extension S : P where T : P {}

func foo(_ fn: (S<String>) -> Void) {}
func bar(_ fn: (P) -> Void) {
  foo(fn)
}
