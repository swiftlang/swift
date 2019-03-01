// RUN: %target-typecheck-verify-swift -swift-version 5

struct S0<T> {
  func foo(_ other: Self) { }
}

class C0<T> {
  func foo(_ other: Self) { }
}

enum E0<T> {
  func foo(_ other: Self) { }
}

// rdar://problem/21745221
struct X {
  typealias T = Int
}

extension X {
  struct Inner {
  }
}

extension X.Inner {
  func foo(_ other: Self) { }
}

// SR-695
class Mario {
  func getFriend() -> Self { return self } // expected-note{{overridden declaration is here}}
  func getEnemy() -> Mario { return self }
}
class SuperMario : Mario {
  override func getFriend() -> SuperMario { // expected-error{{cannot override a Self return type with a non-Self return type}}
    return SuperMario()
  }
  override func getEnemy() -> Self { return self }
}
final class FinalMario : Mario {
    override func getFriend() -> FinalMario {
        return FinalMario()
    }
}

