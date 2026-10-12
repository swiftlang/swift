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

FRTConstructorsTests.test("constructor with default pointer argument") {
  let parent = FRTCtorWithDefaultPointerArg()
  expectNil(parent.parent)
  parent.check()

  let child = FRTCtorWithDefaultPointerArg(parent)
  expectNotNil(child.parent)
  child.check()
}

FRTConstructorsTests.test("constructor with default integer arguments") {
  let a = FRTCtorWithDefaultIntArgs(1, 2, 3)
  expectEqual(a.value, 6)

  let b = FRTCtorWithDefaultIntArgs(1, 2)
  expectEqual(b.value, 126)

  let c = FRTCtorWithDefaultIntArgs(1)
  expectEqual(c.value, 580)
}

FRTConstructorsTests.test("constructor with unsafe default view argument") {
  let a = FRTCtorWithUnsafeDefaultViewArg()
  expectTrue(a.isNull)
}

FRTConstructorsTests.test("class template constructor with default argument") {
  let a = FRTTemplateCtorWithDefaultArgInt(7)
  expectEqual(a.value, 7)
}

runAllTests()
