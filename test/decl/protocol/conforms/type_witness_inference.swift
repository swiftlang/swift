// RUN: %target-typecheck-verify-swift -swift-version 3
// RUN: %target-typecheck-verify-swift -swift-version 4

protocol A {
  associatedtype AT1 = Int
}

protocol B: A {
  associatedtype AT1 = Double
}

protocol C: B {
  associatedtype AT1 = String
}

struct XB : B { }
struct XC : C { }

func wantsDoubleAT1A<T: A>(_: T.Type) where T.AT1 == Double { }
func wantsStringAT1A<T: A>(_: T.Type) where T.AT1 == String { }

func wantsDoubleAT1B<T: B>(_: T.Type) where T.AT1 == Double { }
func wantsStringAT1B<T: B>(_: T.Type) where T.AT1 == String { }

func wantsDoubleAT1C<T: C>(_: T.Type) where T.AT1 == Double { }
func wantsStringAT1C<T: C>(_: T.Type) where T.AT1 == String { }

func testAT1A() {
  wantsDoubleAT1A(XB.self)
  wantsStringAT1A(XC.self)
}

func testAT1B() {
  wantsDoubleAT1B(XB.self)
  wantsStringAT1B(XC.self)
}

func testAT1C() {
  wantsStringAT1C(XC.self)
}
