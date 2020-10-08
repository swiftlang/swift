// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-cxx-interop)
//
// REQUIRES: executable_test

import ClassTemplateNonTypeParameter
import MagicWrapper
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

TemplatesTestSuite.test("typedeffed-non-type-parameter") {
  let pair = MagicIntPair(t: (1, 2))
  expectEqual(pair.t, (1, 2))
}

// TODO(SR-13261): This test doesn't work because Swift doesn't support defaulted generic parameters.
// TemplatesTestSuite.test("defaulted-non-type-parameter") {
//   var intWrapper = IntWrapper(value: 5)
//   var pair = MagicArray<IntWrapper>(t: (intWrapper))
//   expectEqual(pair.t, (intWrapper))
// }

// TODO(SR-13261): This test doesn't work because Swift only expects types as generic arguments.
// TemplatesTestSuite.test("non-type-parameter") {
//   var intWrapper = IntWrapper(value: 5)
//   var pair = MagicArray<Element, intWrapper>(data: (Element(id: 1), Element(id: 2), Element(id: 3), Element(id: 4), Element(id: 5)))
//   expectEqual(pair.data.count, 5)
//   expectEqual(pair.data.3.id, 3)
// }

runAllTests()
