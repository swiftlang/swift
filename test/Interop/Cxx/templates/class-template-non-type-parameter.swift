// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop -enable-experimental-feature ModernImportedCArrays -DMODERN_C_ARRAYS -target %target-has-inline-array-triple)
//
// REQUIRES: executable_test
// REQUIRES: swift_feature_ModernImportedCArrays

import ClassTemplateNonTypeParameter
import StdlibUnittest

var TemplatesTestSuite = TestSuite("TemplatesTestSuite")

#if MODERN_C_ARRAYS
extension InlineArray: @retroactive Equatable where Element: Equatable {
  public static func == (lhs: Self, rhs: Self) -> Bool {
    for i in 0..<count {
      if lhs[i] != rhs[i] { return false }
    }
    return true
  }
}
#endif

TemplatesTestSuite.test("typedeffed-non-type-parameter") {
  #if MODERN_C_ARRAYS
  let pair = MagicIntPair(t: [1, 2])
  expectEqual(pair.t, [1, 2])

  let triple = MagicIntTriple(t: [1, 2, 3])
  expectEqual(triple.t, [1, 2, 3])
  #else
  let pair = MagicIntPair(t: (1, 2))
  expectEqual(pair.t, (1, 2))

  let triple = MagicIntTriple(t: (1, 2, 3))
  expectEqual(triple.t, (1, 2, 3))
  #endif
}

// TODO: This test doesn't work because Swift doesn't support defaulted generic
// parameters (https://github.com/apple/swift/issues/55701).
// TemplatesTestSuite.test("defaulted-non-type-parameter") {
//   var intWrapper = IntWrapper(value: 5)
//   var pair = MagicArray<IntWrapper>(t: (intWrapper))
//   expectEqual(pair.t, (intWrapper))
// }

// TODO: This test doesn't work because Swift only expects types as generic
// arguments (https://github.com/apple/swift/issues/55701).
// TemplatesTestSuite.test("non-type-parameter") {
//   var pair = MagicArray<IntWrapper, 5>(
//     data: (
//       IntWrapper(value: 0), IntWrapper(value: 1), IntWrapper(value: 2), IntWrapper(value: 3),
//       IntWrapper(value: 4)
//     ))
//   expectEqual(pair.count, 5)
//   expectEqual(pair.3.value, 3)
// }

runAllTests()
