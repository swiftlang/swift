// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -Xfrontend -disable-availability-checking)
// REQUIRES: executable_test

import FRTConstructors
import StdlibUnittest

func clone<T: Copyable>(_ t: T) -> (T, T) { return (t, t) }

var FRTConstructorsTests = TestSuite("Calling synthesized foreign reference type initializers")

FRTConstructorsTests.test("explicit defaulted default constructor at +1") {
  let x = FRTExplicitDefaultCtor1()
  let (y, z) = clone(x)
  x.check()
  y.check()
  z.check()
}

FRTConstructorsTests.test("explicit defaulted default constructor at +0") {
  let x = FRTExplicitDefaultCtor0()
  let (y, z) = clone(x)
  x.check()
  y.check()
  z.check()
}

FRTConstructorsTests.test("user-defined default constructor at +1") {
  let x = FRTUserDefaultCtor1()
  let (y, z) = clone(x)
  x.check()
  y.check()
  z.check()
}

FRTConstructorsTests.test("user-defined default constructor at +0") {
  let x = FRTUserDefaultCtor0()
  let (y, z) = clone(x)
  x.check()
  y.check()
  z.check()
}

FRTConstructorsTests.test("constructors with mixed ownership conventions") {
  let a0 = FRTMixedConventionCtors()
  let (a1, a2) = clone(a0)
  a0.check()
  a1.check()
  a2.check()

  let b0 = FRTMixedConventionCtors(0)
  let (b1, b2) = clone(b0)
  b0.check()
  b1.check()
  b2.check()
}

FRTConstructorsTests.test("constructors with mixed ownership conventions, unretained by default") {
  let a0 = FRTMixedConventionCtorsUnretainedByDefault()
  let (a1, a2) = clone(a0)
  a0.check()
  a1.check()
  a2.check()

  let b0 = FRTMixedConventionCtorsUnretainedByDefault(0)
  let (b1, b2) = clone(b0)
  b0.check()
  b1.check()
  b2.check()

  let c0 = FRTMixedConventionCtorsUnretainedByDefault(0, 0)
  let (c1, c2) = clone(c0)
  c0.check()
  c1.check()
  c2.check()
}

runAllTests()
