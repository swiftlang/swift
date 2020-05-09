// RUN: %empty-directory(%t)

// RUN: %target-build-swift -Xfrontend -disable-availability-checking -emit-executable %s -g -o %t/opaque_return_type -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/opaque_return_type -type-from-mangled=%t/input | %FileCheck %s

protocol P {}
extension Int: P {}

func foo() -> some P { return 0 }
var prop: some P { return 0 }

// DEMANGLE: $s18opaque_return_type3fooQryFQOyQo_
// CHECK: some P

// DEMANGLE: $s18opaque_return_type4propQrvpQOyQo_
// CHECK: some P
