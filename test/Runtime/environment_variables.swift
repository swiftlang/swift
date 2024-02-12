// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/main
// RUN: %target-codesign %t/main

// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// RUN: env %env-SWIFT_DEBUG_HELP=YES %env-SWIFT_DEBUG_SOME_UNKNOWN_VARIABLE=42 %env-SWIFT_DEBUG_ENABLE_METADATA_ALLOCATION_ITERATION=YES %env-SWIFT_DEBUG_IMPLICIT_OBJC_ENTRYPOINT=abc %env-SWIFT_DETERMINISTIC_HASHING=whatever %env-SWIFT_ENABLE_MANGLED_NAME_VERIFICATION=YES %env-SWIFT_DEBUG_ENABLE_MALLOC_SCRIBBLE=YES %target-run %t/main >%t.out 2>%t.err
// RUN: cat %t.err %t.out | %FileCheck %s --dump-input fail

// CHECK-DAG: {{Warning: unknown environment variable SWIFT_DEBUG_SOME_UNKNOWN_VARIABLE|Using getenv to read variables. Unknown SWIFT_DEBUG_ variables will not be flagged.}}
// CHECK-DAG: Warning: cannot parse value SWIFT_DEBUG_IMPLICIT_OBJC_ENTRYPOINT=abc, defaulting to 2.
// CHECK-DAG: Warning: cannot parse value SWIFT_DETERMINISTIC_HASHING=whatever, defaulting to false.
// CHECK-DAG: Swift runtime debugging:
// CHECK-DAG:    bool SWIFT_DEBUG_ENABLE_METADATA_ALLOCATION_ITERATION [default: false] - Enable additional metadata allocation tracking for swift-inspect to use.
// CHECK-DAG: uint8_t SWIFT_DEBUG_IMPLICIT_OBJC_ENTRYPOINT [default: 2] - Print warnings when using implicit @objc entrypoints. Set to desired reporting level, 0-3.
// CHECK-DAG:    bool SWIFT_DETERMINISTIC_HASHING [default: false] - Disable randomized hash seeding.
// CHECK-DAG:    bool SWIFT_ENABLE_MANGLED_NAME_VERIFICATION [default: false] - Enable verification that metadata can roundtrip through a mangled name each time metadata is instantiated.
// CHECK-DAG:    bool SWIFT_DEBUG_ENABLE_MALLOC_SCRIBBLE [default: false] - Scribble on runtime allocations such as metadata allocations.
// CHECK-DAG:  string SWIFT_ROOT [default: "{{[^"]*}}"] - Overrides the root directory of the Swift installation. This is used to locate auxiliary files relative to the runtime itself.

// We need to do this because the DAG checks require a non-DAG check as an
// anchor:

// CHECK: Hello, world
print("\nHello, world\n")
