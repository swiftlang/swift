// RUN: %target-typecheck-verify-swift

final class A<T> {
  init(_: T) {}
}

extension A: ExpressibleByNilLiteral where T: ExpressibleByNilLiteral {
  convenience init(nilLiteral: ()) {
    self.init(nil)
  }
}

struct B {
  var foo: A<B?> = A(nil)
}
