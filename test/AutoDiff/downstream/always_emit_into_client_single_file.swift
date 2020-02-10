// TODO(TF-1103): Fix this test so that there is not a linker error. Then, move this test to
// cross_module_derivative_attr_e2e.swift.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-cross-file-derivative-registration -parse-as-library -emit-module -module-name SingleFileModule -emit-module-path %t/SingleFileModule.swiftmodule -emit-library -o %t/%target-library-name(SingleFileModule) %S/Inputs/always_emit_into_client/SingleFileModule/file.swift
// RUN: not %target-build-swift -I%t -L%t %s -o %t/a.out -lm -lSingleFileModule 2>&1 | %FileCheck %s

import StdlibUnittest

import SingleFileModule

var AlwaysEmitIntoClientTests = TestSuite("AlwaysEmitIntoClient")

AlwaysEmitIntoClientTests.test("registration") {
  expectEqual(10, gradient(at: 0, in: f))
}

runAllTests()

// CHECK: {{[Uu]}}ndefined
// CHECK: AD__$s16SingleFileModule1fyS2fF_PSRS
