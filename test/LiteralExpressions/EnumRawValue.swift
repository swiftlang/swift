// Enum case raw value expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -typecheck -dump-ast %s -enable-experimental-feature LiteralExpressions -verify | %FileCheck %s

enum E1: Int {
    case a = 2 + 2
    case b
    case c
}
// CHECK-LABEL: (enum_element_decl {{.*}} "a" interface_type="(E1.Type) -> E1"
// CHECK: (original_raw_value_expr=binary_expr type="Int" {{.*}} nothrow isolation_crossing="none"
// CHECK: (folded_raw_value_expr=integer_literal_expr implicit type="Int" {{.*}} value="4" builtin_initializer="Swift.(file).Int.init(_builtinIntegerLiteral:)"

// CHECK-LABEL: (enum_element_decl {{.*}} "b" interface_type="(E1.Type) -> E1"
// CHECK: (raw_value_expr=integer_literal_expr implicit type="Int" {{.*}} value="5" builtin_initializer="Swift.(file).Int.init(_builtinIntegerLiteral:)"

// CHECK-LABEL: (enum_element_decl {{.*}} "c" interface_type="(E1.Type) -> E1"
// CHECK: (raw_value_expr=integer_literal_expr implicit type="Int" {{.*}} value="6" builtin_initializer="Swift.(file).Int.init(_builtinIntegerLiteral:)"

@section("mysection") let sectionInt = 42
let someOtherInt = 42
enum E2: Int {
    case life = sectionInt
    case universe = someOtherInt << 1
    case everything
}
// CHECK-LABEL: (enum_element_decl {{.*}} "life" interface_type="(E2.Type) -> E2"
// CHECK: (original_raw_value_expr=declref_expr type="Int" {{.*}} decl="EnumRawValue.(file).sectionInt@{{.*}}EnumRawValue.swift:{{[0-9]+}}:{{[0-9]+}}" function_ref=unapplied)
// CHECK: (folded_raw_value_expr=integer_literal_expr implicit type="Int" {{.*}} value="42" builtin_initializer="Swift.(file).Int.init(_builtinIntegerLiteral:)"

// CHECK-LABEL: (enum_element_decl {{.*}} "universe" interface_type="(E2.Type) -> E2"
// CHECK: (original_raw_value_expr=binary_expr type="Int" {{.*}} nothrow isolation_crossing="none"
// CHECK: (folded_raw_value_expr=integer_literal_expr implicit type="Int" {{.*}} value="84" builtin_initializer="Swift.(file).Int.init(_builtinIntegerLiteral:)"

// CHECK-LABEL: (enum_element_decl {{.*}} "everything" interface_type="(E2.Type) -> E2"
// CHECK: (raw_value_expr=integer_literal_expr implicit type="Int" {{.*}} value="85" builtin_initializer="Swift.(file).Int.init(_builtinIntegerLiteral:)"
