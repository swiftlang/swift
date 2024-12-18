// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(opaque_result_type_runtime_call_other)) -enable-library-evolution %S/Inputs/opaque_result_type_runtime_call_other.swift -emit-module -emit-module-path %t/opaque_result_type_runtime_call_other.swiftmodule -module-name opaque_result_type_runtime_call_other -target %target-swift-5.1-abi-triple
// RUN: %target-codesign %t/%target-library-name(opaque_result_type_runtime_call_other)
// RUN: %target-build-swift %s -lopaque_result_type_runtime_call_other -I %t -L %t -o %t/main %target-rpath(%t) -target %target-swift-5.1-abi-triple
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(opaque_result_type_runtime_call_other) | %FileCheck %s
// REQUIRES: executable_test

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import opaque_result_type_runtime_call_other

extension String: P {}

func caller<T: P>(t: T) {
  print(callee(t: t))
}

// CHECK: Hello, world
caller(t: "Hello, world")
