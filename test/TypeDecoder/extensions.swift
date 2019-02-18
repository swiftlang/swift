// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/extensions -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/extensions -type-from-mangled=%t/input | %FileCheck %s

struct Concrete {}

extension Concrete {
  struct Nested {}
}

struct Generic<T> {}

protocol Proto {}

extension Generic where T : Proto {
  struct Nested1 {}
}

extension Generic where T == Int {
  struct Nested2 {}
}

// DEMANGLE: $s10extensions8ConcreteV6NestedVD
// CHECK: Concrete.Nested

// DEMANGLE: $s10extensions7GenericVA2A5ProtoRzlE7Nested1Vyx_GD
// CHECK: Generic<τ_0_0>.Nested1

// DEMANGLE: $s10extensions7GenericVAASiRszlE7Nested2VySi_GD
// CHECK: Generic<Int>.Nested2
