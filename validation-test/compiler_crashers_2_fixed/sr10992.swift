// RUN: not %target-swift-frontend -typecheck %s

protocol P {}
struct S<T> {}
extension S : P where T : P {}

func foo(_ fn: (S<String>) -> Void) {}
func bar(_ fn: (P) -> Void) {
  foo(fn)
}
