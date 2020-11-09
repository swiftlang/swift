// RUN: %target-swift-frontend -emit-sil %s

// This is an integration test to ensure that SILGen, DI and the ownership
// verifier support the SIL we generate for stored properties with initial
// values.

enum E {
  case foo(Any)
}

struct S<T, U : BinaryInteger> {
  var x1: T? = nil
  var x2 = 0 as! T
  var x3 = E.foo(0)
  var x4: (Int, Int, Int, Int) = (0, 0, 0, 0)
  var x5: (U, U, U, U) = (0, 0, 0, 0)

  init() {}
}

class C<T, U : BinaryInteger> {
  var x1: T? = nil
  var x2 = 0 as! T
  var x3 = E.foo(0)
  var x4: (Int, Int, Int, Int) = (0, 0, 0, 0)
  var x5: (U, U, U, U) = (0, 0, 0, 0)

  init() {}
}
