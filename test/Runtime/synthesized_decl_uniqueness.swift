// RUN: %empty-directory(%t)
// RUN: %target-build-swift -parse-as-library -force-single-frontend-invocation %S/Inputs/synthesized_decl_uniqueness.swift -emit-object -o %t/A.o -module-name A -emit-module-path %t/A.swiftmodule
// RUN: %target-build-swift -parse-as-library -force-single-frontend-invocation %S/Inputs/synthesized_decl_uniqueness.swift -emit-object -o %t/B.o -module-name B -emit-module-path %t/B.swiftmodule
// RUN: %target-build-swift -I %t %s %t/A.o %t/B.o -o %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import A
import B

var tests = TestSuite("metadata identity for synthesized types")

tests.test("synthesized type identity across modules") {
  expectEqual(A.getCLError(), B.getCLError())
  expectEqual(A.getCLErrorCode(), B.getCLErrorCode())
}

runAllTests()
