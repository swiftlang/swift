// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-library-evolution | %FileCheck %s

public enum Foo {
  case foo(bar: Int)
}

public let f = Foo.foo(bar: 1)

// CHECK: @"$s23enum_case_symbol_compat3FooO3fooyACSi3bar_tcACmFWC" = alias [[INT:i32|i64]], [[INT:i32|i64]]* @"$s23enum_case_symbol_compat3FooO3fooyACSi_tcACmFWC"
