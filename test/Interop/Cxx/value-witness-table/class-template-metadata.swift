// RUN: %target-run-simple-swift(-I %S/Inputs -Xfrontend -enable-experimental-cxx-interop)
//
// REQUIRES: executable_test

import ClassTemplateMetadata
import StdlibUnittest

var ClassTemplateMetadataTestSuite = TestSuite("Class Template Metadata")

func cmpMetadata<T, U>(_ _: T, _ _: U) -> Bool { "\(T.self)" == "\(U.self)" }

ClassTemplateMetadataTestSuite.test("Different specializations of a class template have different metadata.") {
  let a = Spec1()
  let b = Spec2()

  expectFalse(cmpMetadata(a, b))
  expectTrue(cmpMetadata(a, a))
}

runAllTests()
