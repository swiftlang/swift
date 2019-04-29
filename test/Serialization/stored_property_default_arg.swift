// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-testing -o %t %S/Inputs/stored_property_default_arg_testable.swift
// RUN: %target-swift-frontend -emit-silgen -I %t %s | %FileCheck %s

@testable import stored_property_default_arg_testable

// CHECK: function_ref default argument 0 of Foo.init(bar:baz:)
// CHECK-NEXT: [[BAR_REF:%.*]] = function_ref @$s36stored_property_default_arg_testable3FooV3bar3bazACSi_SbtcfcfA_ : $@convention(thin) () -> Int
// CHECK-NEXT: [[BAR:%.*]] = apply [[BAR_REF]]() : $@convention(thin) () -> Int
// CHECK-NEXT: function_ref Foo.init(bar:baz:)
// CHECK-NEXT: [[FOO_REF:%.*]] = function_ref @$s36stored_property_default_arg_testable3FooV3bar3bazACSi_SbtcfC : $@convention(method) (Int, Bool, @thin Foo.Type) -> Foo
// CHECK-NEXT: {{.*}} = apply [[FOO_REF]]([[BAR]], {{.*}}, {{.*}}) : $@convention(method) (Int, Bool, @thin Foo.Type) -> Foo

_ = Foo(baz: true)
