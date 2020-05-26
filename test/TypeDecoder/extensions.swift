// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/extensions -emit-module

// RUN: sed -ne '/\/\/ *DEMANGLE-TYPE: /s/\/\/ *DEMANGLE-TYPE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/extensions -type-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-TYPE

// RUN: sed -ne '/\/\/ *DEMANGLE-DECL: /s/\/\/ *DEMANGLE-DECL: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/extensions -decl-from-mangled=%t/input | %FileCheck %s --check-prefix=CHECK-DECL

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

// DEMANGLE-TYPE: $s10extensions8ConcreteV6NestedVD
// CHECK-TYPE: Concrete.Nested

// DEMANGLE-TYPE: $s10extensions7GenericVA2A5ProtoRzlE7Nested1Vyx_GD
// CHECK-TYPE: Generic<τ_0_0>.Nested1

// DEMANGLE-TYPE: $s10extensions7GenericVAASiRszlE7Nested2VySi_GD
// CHECK-TYPE: Generic<Int>.Nested2

// DEMANGLE-DECL: $s10extensions8ConcreteV6NestedV
// CHECK-DECL: extensions.(file).Concrete extension.Nested

// DEMANGLE-DECL: $s10extensions7GenericVA2A5ProtoRzlE7Nested1V
// CHECK-DECL: extensions.(file).Generic extension.Nested1

// DEMANGLE-DECL: $s10extensions7GenericVAASiRszlE7Nested2V
// CHECK-DECL: extensions.(file).Generic extension.Nested2

// Layout Constraints
// FIXME: When other layout constraints are allowed in source level swift, test
// that we correctly demangle those as well.

extension Generic where T: AnyObject {
  struct NestedViaAnyObject {}
}

// DEMANGLE-TYPE: $s10extensions7GenericVAARlzClE18NestedViaAnyObjectVyx_GD
// CHECK-TYPE: Generic<τ_0_0>.NestedViaAnyObject

// DEMANGLE-DECL: $s10extensions7GenericVAARlzClE18NestedViaAnyObjectV
// CHECK-DECL: extensions.(file).Generic extension.NestedViaAnyObject

