// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/structural_types -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/structural_types -type-from-mangled=%t/input | %FileCheck %s


// If this were supported, it would be `(t: τ_0_0...)`
// But tuples with a variadic 'd' marker are not actually valid Swift types, so
// the type decoder rejects them.
// DEMANGLE: $sx1td_t
// CHECK: Can't resolve type of $sx1td_t
