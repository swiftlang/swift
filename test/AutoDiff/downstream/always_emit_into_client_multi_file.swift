// TODO(TF-1103): Fix this test so that there is not a linker error. Then, move this test to
// cross_module_derivative_attr_e2e.swift.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-cross-file-derivative-registration -parse-as-library -emit-module -module-name MultiFileModule -emit-module-path %t/MultiFileModule.swiftmodule -emit-library -o %t/%target-library-name(MultiFileModule) %S/Inputs/always_emit_into_client/MultiFileModule/file1.swift %S/Inputs/always_emit_into_client/MultiFileModule/file2.swift
// RUN: not %target-build-swift -I%t -L%t %s -o %t/a.out -lm -lMultiFileModule 2>&1 | %FileCheck %s

import StdlibUnittest

import MultiFileModule

var AlwaysEmitIntoClientTests = TestSuite("AlwaysEmitIntoClient")

AlwaysEmitIntoClientTests.test("registration") {
  expectEqual(10, gradient(at: 0, in: f))
}

runAllTests()

// CHECK: {{[Uu]}}ndefined
// CHECK: AD__$s15MultiFileModule1fyS2fF_PSRS
