// RUN: %target-swift-frontend -emit-silgen %s

// Make sure an unbound method reference which is then immediately
// called can be SILGen'd correctly.

class C {
  func foo(x: Int) {}
}

protocol P {
  func foo(x: Int)
}

let c = C()

func doIt(_ c: C, _ p: P ) {
  _ = C.foo
  _ = C.foo(c)
  _ = C.foo(c)(x: 123)

  _ = P.foo
  _ = P.foo(p)
  _ = P.foo(p)(x: 123)
}
