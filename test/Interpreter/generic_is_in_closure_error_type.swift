// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) -O -target %target-future-triple -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule %S/Inputs/generic_is_in_closure_error_type_lib.swift
// RUN: %target-codesign %t/%target-library-name(Lib)
// RUN: %target-build-swift %s -O -target %target-future-triple -I %t -L %t -lLib -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(Lib) | %FileCheck %s

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// An `is` test against a generic `Error` type inside a closure that the
// optimizer inlines into its generic caller, compiled into a separate module so
// the client cannot specialize it. This used to hand the runtime an unresolved
// type and crash in tryCast(); see
// test/SILOptimizer/function_signature_opts_test_only_cast_generic_error.swift.

import Lib

struct X: Error {}
struct Y: Error {}
enum Z: Error { case a }

// CHECK: true
print(check(X.self) { throw X() })
// CHECK: false
print(check(Y.self) { throw X() })
// CHECK: true
print(check(Z.self) { throw Z.a })
// CHECK: true
print(check((any Error).self) { throw X() })
// CHECK: false
print(check(X.self) { })
