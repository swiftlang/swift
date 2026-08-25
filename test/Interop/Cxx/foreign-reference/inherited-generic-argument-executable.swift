// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ForeignReferenceTypeInheritance -Xfrontend -disable-availability-checking)

// Miscompiles due to unrelated SIL type lowering issue
// UNSUPPORTED: OS=linux-gnu
// REQUIRES: executable_test
// REQUIRES: swift_feature_ForeignReferenceTypeInheritance

import InheritedGenericArgument
import StdlibUnittest

var Tests = TestSuite("InheritedGenericArgument")

func classify<T: BaseObj>(_ obj: T) -> CInt { obj.tag() }

Tests.test("base-constrained generic with derived FRT argument") {
  expectEqual(1, classify(DerivedObj1.make()))
  expectEqual(2, classify(DerivedObj2.make()))
}

runAllTests()
