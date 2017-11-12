// RUN: %target-typecheck-verify-swift -swift-version 5

struct S0<T> {
  func foo(_ other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'S0'?}}{{21-25=S0}}
}

class C0<T> {
  func foo(_ other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'C0'?}}{{21-25=C0}}
}

enum E0<T> {
  func foo(_ other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'E0'?}}{{21-25=E0}}
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
  func foo(_ other: Self) { } // expected-error{{'Self' is only available in a protocol or as the result of a method in a class; did you mean 'X.Inner'?}}{{21-25=X.Inner}}
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

