// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Lib)) -O -target %target-future-triple -enable-experimental-feature NoncopyableCasting -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule %S/Inputs/generic_is_in_closure_lib.swift
// RUN: %target-codesign %t/%target-library-name(Lib)
// RUN: %target-build-swift %s -O -target %target-future-triple -enable-experimental-feature NoncopyableCasting -I %t -L %t -lLib -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// An `is` test whose target mentions a generic parameter, inside a closure the
// optimizer inlines into its generic caller, in a separate module so the client
// cannot specialize it. This used to hand the runtime an unresolved type and
// crash in tryCast(); see
// test/SILOptimizer/function_signature_opts_test_only_cast_generic.swift.

import Lib

// CHECK: true false false
print(check(Box(1), Int.self), check(Box("s"), Int.self), check(Box(1), String.self))
