// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift -Xfrontend -disable-availability-checking)

// Temporarily disable when running with an older runtime (rdar://128681137)
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import PassAsParameter

var PassAsParameterTestSuite = TestSuite("Passing foreign reference type as parameter")

PassAsParameterTestSuite.test("pass as pointer") {
  let a = IntBox.create(123)
  let aValue = extractValueFromPtr(a)
  expectEqual(aValue, 123)
}

PassAsParameterTestSuite.test("pass as reference") {
  let a = IntBox.create(321)!
  let aValue = extractValueFromRef(a)
  expectEqual(aValue, 321)
}

PassAsParameterTestSuite.test("pass as const reference") {
  let a = IntBox.create(321)!
  let aValue = extractValueFromConstRef(a)
  expectEqual(aValue, 321)
}

PassAsParameterTestSuite.test("pass as reference to pointer") {
  var a = IntBox.create(123)
  let aValue = extractValueFromRefToPtr(&a)
  expectEqual(aValue, 123)
}

PassAsParameterTestSuite.test("pass as const reference to pointer") {
  let a = IntBox.create(456)
  let aValue = extractValueFromConstRefToPtr(a)
  expectEqual(aValue, 456)
}

PassAsParameterTestSuite.test("pass as const reference to pointer") {
  var a = IntBox.create(654)
  let aValue = extractValueFromConstRefToPtr(a)
  expectEqual(aValue, 654)
}

PassAsParameterTestSuite.test("pass as const reference to const pointer") {
  var a = IntBox.create(789)
  let aValue = extractValueFromConstRefToConstPtr(a)
  expectEqual(aValue, 789)
}

runAllTests()
