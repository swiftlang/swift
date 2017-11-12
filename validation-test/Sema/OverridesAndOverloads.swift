// RUN: %target-typecheck-verify-swift -D ERRORS
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


struct Token1 {}
struct Token2 {}
struct Token3 {}

class C1 {}
class C1x : C1 {}
class C1xx : C1x {}

protocol P1 {}
protocol P1x : P1 {}

struct P1ImplS1 : P1 {}
struct P1xImplS1 : P1x {}

class P1ImplC1x : C1, P1 {}
class P1ImplC1xx {}
class P1xImplC1x : C1, P1x {}

var which = ""

var Overrides = TestSuite("Overrides")

Overrides.test("covariant argument override, non-optional to optional") {
  class Base {
    func foo(_: C1) { which = "Base.foo(C1)" }
  }
  class Derived : Base {
    override func foo(_: C1?) { which = "Derived.foo(C1?)" }
  }

  Derived().foo(C1())
  expectEqual("Derived.foo(C1?)", which)

  Derived().foo(nil as C1?)
  expectEqual("Derived.foo(C1?)", which)
}

Overrides.test("covariant argument override, derived class to base class") {
  class Base {
    func foo(_: C1x) { which = "Base.foo(C1x)" }
  }
  class Derived : Base {
    override func foo(_: C1) { which = "Derived.foo(C1)" }
  }

  Derived().foo(C1())
  expectEqual("Derived.foo(C1)", which)

  Derived().foo(C1x())
  expectEqual("Derived.foo(C1)", which)
}

Overrides.test("covariant argument override, optional derived class to non-optional base class") {
  class Base {
    func foo(_: C1x) { which = "Base.foo(C1x)" }
  }
  class Derived : Base {
    override func foo(_: C1?) { which = "Derived.foo(C1?)" }
  }

  Derived().foo(C1())
  expectEqual("Derived.foo(C1?)", which)

  Derived().foo(C1x())
  expectEqual("Derived.foo(C1?)", which)

  Derived().foo(nil)
  expectEqual("Derived.foo(C1?)", which)
}

Overrides.test("covariant argument override, protocol to protocol") {
  // FIXME: https://bugs.swift.org/browse/SR-731
  // Covariant overrides don't work with protocols
  class Base {
    func foo(_: P1x) { which = "Base.foo(P1x)" }
  }
  class Derived : Base {
    /*FIXME: override */ func foo(_: P1) { which = "Derived.foo(P1)" }
  }

  Derived().foo(P1ImplS1())
  expectEqual("Derived.foo(P1)", which)

  Derived().foo(P1xImplS1())
  expectEqual("Base.foo(P1x)", which)
}

Overrides.test("covariant argument override, struct to protocol") {
  // FIXME: https://bugs.swift.org/browse/SR-731
  // Covariant overrides don't work with protocols
  class Base {
    func foo(_: P1ImplS1) { which = "Base.foo(P1ImplS1)" }
  }
  class Derived : Base {
    /*FIXME: override */ func foo(_: P1) { which = "Derived.foo(P1)" }
  }

  // FIXME: https://bugs.swift.org/browse/SR-731
  expectFailure {
    Derived().foo(P1ImplS1())
    expectEqual("Derived.foo(P1)", which)
  }

  Derived().foo(P1xImplS1())
  expectEqual("Derived.foo(P1)", which)
}

Overrides.test("contravariant return type override, optional to non-optional") {
  class Base {
    func foo() -> C1? { which = "Base.foo() -> C1?"; return C1() }
  }
  class Derived : Base {
    override func foo() -> C1 {
      which = "Derived.foo() -> C1"; return C1()
    }
  }

  _ = Derived().foo() as C1
  expectEqual("Derived.foo() -> C1", which)

  _ = Derived().foo() as C1?
  expectEqual("Derived.foo() -> C1", which)
}

Overrides.test("contravariant return type override, base class to derived class") {
  class Base {
    func foo() -> C1 { which = "Base.foo() -> C1"; return C1() }
  }
  class Derived : Base {
    override func foo() -> C1x {
      which = "Derived.foo() -> C1x"; return C1x()
    }
  }

  _ = Derived().foo() as C1
  expectEqual("Derived.foo() -> C1x", which)

  _ = Derived().foo() as C1x
  expectEqual("Derived.foo() -> C1x", which)
}

Overrides.test("contravariant return type override, optional base class to non-optional derived class") {
  class Base {
    func foo() -> C1? { which = "Base.foo() -> C1?"; return C1() }
  }
  class Derived : Base {
    override func foo() -> C1x {
      which = "Derived.foo() -> C1x"; return C1x()
    }
  }

  _ = Derived().foo() as C1
  expectEqual("Derived.foo() -> C1x", which)

  _ = Derived().foo() as C1x
  expectEqual("Derived.foo() -> C1x", which)
}

Overrides.test("contravariant return type override, protocol to protocol") {
  // FIXME: https://bugs.swift.org/browse/SR-733
  // Contravariant overrides on return type don't work with protocols
  class Base {
    func foo() -> P1 { which = "Base.foo() -> P1"; return P1ImplS1() }
  }
  class Derived : Base {
    /*FIXME: override */ func foo() -> P1x {
      which = "Derived.foo() -> P1x"; return P1xImplS1()
    }
  }

  // https://bugs.swift.org/browse/SR-733
  // FIXME: uncomment when the bug is fixed.
  // Derived().foo() as P1 // error: ambiguous use of 'foo()'
  // expectEqual("Derived.foo() -> P1x", which)

  _ = Derived().foo() as P1x
  expectEqual("Derived.foo() -> P1x", which)
}

Overrides.test("contravariant return type override, protocol to struct") {
  // FIXME: https://bugs.swift.org/browse/SR-733
  // Contravariant overrides on return type don't work with protocols
  class Base {
    func foo() -> P1 { which = "Base.foo() -> P1"; return P1ImplS1() }
  }
  class Derived : Base {
    /*FIXME: override */ func foo() -> P1ImplS1 {
      which = "Derived.foo() -> P1ImplS1"; return P1ImplS1()
    }
  }

  // https://bugs.swift.org/browse/SR-733
  // FIXME: uncomment when the bug is fixed.
  // Derived().foo() as P1 // error: ambiguous use of 'foo()'
  // expectEqual("Derived.foo() -> P1ImplS1", which)

  _ = Derived().foo() as P1ImplS1
  expectEqual("Derived.foo() -> P1ImplS1", which)
}

Overrides.test("overrides don't affect overload resolution") {
  class Base {
    func foo(_: P1) { which = "Base.foo(P1)" }
  }
  class Derived : Base {
    func foo(_: P1x) { which = "Derived.foo(P1x)" }
  }
  class MoreDerived : Derived {
    override func foo(_: P1) { which = "MoreDerived.foo(P1)" }
  }

  Base().foo(P1ImplS1())
  expectEqual("Base.foo(P1)", which)

  Derived().foo(P1ImplS1())
  expectEqual("Base.foo(P1)", which)

  Derived().foo(P1xImplS1())
  expectEqual("Derived.foo(P1x)", which)

  MoreDerived().foo(P1ImplS1())
  expectEqual("MoreDerived.foo(P1)", which)

  MoreDerived().foo(P1xImplS1())
  expectEqual("Derived.foo(P1x)", which)
}

var Overloads = TestSuite("Overloads")

Overloads.test("perfect match") {
  class Base {
    func foo(_: C1,  _: C1,  _: C1)  { which = "C1, C1, C1" }
    func foo(_: C1,  _: C1,  _: C1x) { which = "C1, C1, C1x" }
    func foo(_: C1,  _: C1x, _: C1)  { which = "C1, C1x, C1" }
    func foo(_: C1,  _: C1x, _: C1x) { which = "C1, C1x, C1x" }
    func foo(_: C1x, _: C1,  _: C1)  { which = "C1x, C1, C1" }
    func foo(_: C1x, _: C1,  _: C1x) { which = "C1x, C1, C1x" }
    func foo(_: C1x, _: C1x, _: C1)  { which = "C1x, C1x, C1" }
    func foo(_: C1x, _: C1x, _: C1x) { which = "C1x, C1x, C1x" }
  }

  Base().foo(C1(), C1(), C1());    expectEqual("C1, C1, C1", which)
  Base().foo(C1(), C1(), C1x());   expectEqual("C1, C1, C1x", which)
  Base().foo(C1(), C1x(), C1());   expectEqual("C1, C1x, C1", which)
  Base().foo(C1(), C1x(), C1x());  expectEqual("C1, C1x, C1x", which)
  Base().foo(C1x(), C1(), C1());   expectEqual("C1x, C1, C1", which)
  Base().foo(C1x(), C1(), C1x());  expectEqual("C1x, C1, C1x", which)
  Base().foo(C1x(), C1x(), C1());  expectEqual("C1x, C1x, C1", which)
  Base().foo(C1x(), C1x(), C1x()); expectEqual("C1x, C1x, C1x", which)
}

Overloads.test("implicit conversion to superclass") {
  class Base {
    func foo(_: C1) { which = "foo(C1)" }
  }

  Base().foo(C1()); expectEqual("foo(C1)", which)
  Base().foo(C1x()); expectEqual("foo(C1)", which)
  Base().foo(C1xx()); expectEqual("foo(C1)", which)
}

Overloads.test("implicit conversion to protocol existential") {
  class Base {
    func foo(_: P1) { which = "foo(P1)" }
  }

  Base().foo(P1ImplS1()); expectEqual("foo(P1)", which)
  Base().foo(P1xImplS1()); expectEqual("foo(P1)", which)
}

Overloads.test("implicit conversion to a superclass is better than conversion to an optional") {
  class Base {
    func foo(_: C1) { which = "foo(C1)" }
    func foo(_: C1x?) { which = "foo(C1x?)" }
  }

  Base().foo(C1()); expectEqual("foo(C1)", which)
  Base().foo(C1x()); expectEqual("foo(C1)", which)
  Base().foo(C1xx()); expectEqual("foo(C1)", which)

  Base().foo(C1x() as C1x?); expectEqual("foo(C1x?)", which)
  Base().foo(C1xx() as C1xx?); expectEqual("foo(C1x?)", which)
}

Overloads.test("implicit conversion to a protocol existential is better than conversion to an optional") {
  class Base {
    func foo(_: P1) { which = "foo(P1)" }
    func foo(_: P1x?) { which = "foo(P1x?)" }
  }

  Base().foo(P1ImplS1()); expectEqual("foo(P1)", which)
  Base().foo(P1xImplS1()); expectEqual("foo(P1)", which)

  Base().foo(P1xImplS1() as P1x?); expectEqual("foo(P1x?)", which)
}

Overloads.test("implicit conversion to a superclass is ambiguous with conversion to a protocol existential") {
  class Base {
    func foo(_: C1) { which = "foo(C1)" } // expected-note {{found this candidate}}
    func foo(_: P1) { which = "foo(P1)" } // expected-note {{found this candidate}}
  }

  Base().foo(C1());       expectEqual("foo(C1)", which)
  Base().foo(P1ImplS1()); expectEqual("foo(P1)", which)

#if ERRORS
  Base().foo(P1ImplC1x())
  // expected-error @-1 {{ambiguous use of 'foo'}}
#endif
}

Overloads.test("generic methods are worse than non-generic") {
  class Base {
    func foo(_: C1) { which = "foo(C1)" }
    func foo(_: Any) { which = "foo(Any)" }
    func foo<T>(_: T) { which = "foo(T)" }
    // It is not possible to call foo<T>(T).  foo(Any) always wins.

    func bar(_: C1) { which = "bar(C1)" }
    func bar<T>(_: T) { which = "bar(T)" }
  }

  Base().foo(C1());     expectEqual("foo(C1)", which)
  Base().foo(Token1()); expectEqual("foo(Any)", which)

  Base().bar(C1());     expectEqual("bar(C1)", which)
  Base().bar(Token1()); expectEqual("bar(T)", which)
}

runAllTests()

