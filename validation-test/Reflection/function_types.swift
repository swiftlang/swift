// Target Swift 5.5 so functions with global actors are encoded as a proper
// mangled name, not a function accessor. Remote Mirror can't call function
// accessors and will fail to read the type.
// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.5-abi-triple -lswiftSwiftReflectionTest %s -o %t/function_types
// RUN: %target-codesign %t/function_types

// RUN: %target-run %target-swift-reflection-test %t/function_types | %FileCheck %s --check-prefix=CHECK

// REQUIRES: reflection_test_support
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: asan

import SwiftReflectionTest

struct S {
  var f = { @MainActor in }
}

// This Task is necessary to ensure that the concurrency runtime is brought in.
// Without that, the type lookup for @MainActor may fail.
Task {}

// CHECK: Type reference:
// CHECK: (struct function_types.S)

// CHECK: Type info:
// CHECK: (struct size=
// CHECK:   (field name=f offset=0
// CHECK:     (thick_function size=
// CHECK:       (field name=function offset=0
// CHECK:         (builtin size=
// CHECK:       (field name=context
// CHECK:         (reference kind=strong refcounting=native)))))
// CHECK: Mangled name: $s14function_types1SV
// CHECK: Demangled name: function_types.S
reflect(any: S())

// CHECK: Type reference:
// CHECK: (function
// CHECK:   (global-actor
// CHECK:       (class Swift.MainActor))
// CHECK:       (parameters)
// CHECK:       (result
// CHECK:         (tuple))

// CHECK: Type info:
// CHECK: (thick_function size=
// CHECK:   (field name=function offset=0
// CHECK:     (builtin size=
// CHECK:   (field name=context offset=
// CHECK:     (reference kind=strong refcounting=native)))
// CHECK: Mangled name: $syyScMYcc
// CHECK: Demangled name: @Swift.MainActor () -> ()
reflect(any: S().f)

doneReflecting()

