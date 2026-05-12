// Constant globals using @section initialized with literal expressions
// REQUIRES: swift_feature_LiteralExpressions
// REQUIRES: OS=macosx
// RUN: %target-swift-frontend -typecheck -dump-ast %s -enable-experimental-feature LiteralExpressions -target %target-cpu-apple-macosx15.0 -verify | %FileCheck %s

@section("mysection") let largeInt: Int128 = 128 + 2
// CHECK-LABEL: (pattern_named type="Int128" "largeInt")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int128" location={{.*}}LargeInt.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="130"

@section("mysection") let largeUInt: UInt128 = 128 + 2
// CHECK-LABEL: (pattern_named type="UInt128" "largeUInt")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="UInt128" location={{.*}}LargeInt.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="130"
