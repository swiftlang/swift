// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

let conformanceUniquingTests = TestSuite("ConformanceUniquing")

func isSimpleSetAlgebra<T: SetAlgebra>(_: T.Type) -> Bool {
  return T.self == T.Element.self
}

// rdar://problem/46685973
conformanceUniquingTests.test("Nested types with the same name") {
  expectTrue(isSimpleSetAlgebra(NSData.WritingOptions.self))
  expectTrue(isSimpleSetAlgebra(JSONSerialization.WritingOptions.self))
}

runAllTests()
