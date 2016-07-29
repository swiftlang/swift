// RUN: %target-parse-verify-swift

struct S0 {
  func foo0() -> Self { return self }
  func foo1(_ a: Self) { }
}
class C0 {
  func foo0() -> Self { return self }
  func foo1(_ a: Self) { }
}
enum E0 {
  func foo0() -> Self { return self }
  func foo1(_ a: Self) { }
}
// Test generics
struct S1<T> {
  func foo0(_ other: Self) {
    _ = Self.self // check lookup of 'Self' metatype
  }
  func foo1() -> Self { return self }
}
class C1<T> {
  func foo0(_ other: Self) { }
  func foo1() -> Self { return self }
}
enum E1<T> {
  func foo0(_ other: Self) { }
  func foo1() -> Self { return self }
}

// rdar://problem/21745221
struct X {
  typealias T = Int
}

extension X {
  struct Inner {
    func foo0() -> Self { return self }
  }
}

extension X.Inner {
  func foo1(_ other: Self) { }
}

// Test we are still using dynamic, not concrete, 'Self'
// for class and protocol methods which return 'Self'

class NonFinal {
  func reference() -> Self {
    return self
  }
}
final class Final : NonFinal {
}
protocol Proto {
  func copy() -> Self
}
struct Conf : Proto {
  func copy() -> Self {
    return self
  }
}

func testClass() {
  let f0 = Final()
  let _: Final = f0.reference()
}
func testProto() {
  let p0 = Conf()
  let _: Conf = p0.copy()
}

let _: Self = () // expected-error{{'Self' is only available in a type}}
func noParent() -> Self { fatalError() } // expected-error{{global function cannot return 'Self'}}

_ = Self.self // expected-error {{'Self' is only available in a type}}

