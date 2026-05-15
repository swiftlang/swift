// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -cxx-interoperability-mode=default)
//
// REQUIRES: executable_test

import ClassTemplateInNamespace
import StdlibUnittest

var ClassTemplateInNamespace = TestSuite("ClassName")

ClassTemplateInNamespace.test("SameNameDifferentNamespaces") {
  let p1: Any = NS1.BoxInt()
  let p2: Any = NS2.BoxInt()
  expectFalse(type(of: p1) == type(of: p2))
  let p: Any = NS1.BoxInt()
  expectTrue(type(of: p1) == type(of: p))
}

ClassTemplateInNamespace.test("DeeplyNestedNamespaces") {
  let c1: Any = A.B.C1.BoxInt()
  let c2: Any = A.B.C2.BoxInt()
  expectFalse(type(of: c1) == type(of: c2))

  let b: Any = A.B.BoxInt()
  expectFalse(type(of: c1) == type(of: b))
}

ClassTemplateInNamespace.test("DifferentSpecializations") {
  let c1: Any = A.B.C1.BoxInt()
  let b: Any = A.B.BoxInt()
  let bb: Any = A.B.BoxOfBoxInt()
  expectFalse(type(of: c1) == type(of: bb))
  expectFalse(type(of: b) == type(of: bb))

  let boxC1: Any = A.B.BoxOfBoxC1()
  let boxC2: Any = A.B.BoxOfBoxC2()
  expectFalse(type(of: boxC1) == type(of: boxC2))

  let c1BoxOfBoxInt: Any = A.B.C1.BoxOfBoxInt()
  let c1BoxOfBoxC1: Any = A.B.C1.BoxOfBoxC1()
  expectTrue(type(of: c1BoxOfBoxInt) == type(of: c1BoxOfBoxC1))
  expectFalse(type(of: boxC1) == type(of: c1BoxOfBoxInt))
  expectFalse(type(of: boxC1) == type(of: c1BoxOfBoxC1))
}

runAllTests()
