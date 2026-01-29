// Constant globals using @section initialized with literal expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -typecheck -dump-ast %s -enable-experimental-feature LiteralExpressions | %FileCheck %s

// Simple arithmetic operators on integers
@section("mysection") let intBinaryArithOp1 = 1 + 1
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp1")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:6:47 range=[{{.*}}] value="2"

@section("mysection") let intBinaryArithOp2 = 4 - 1
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp2")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:10:47 range=[{{.*}}] value="3"

@section("mysection") let intBinaryArithOp3 = 3 * 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp3")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:14:47 range=[{{.*}}] value="6"

@section("mysection") let intBinaryArithOp4 = 4 / 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp4")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:18:47 range=[{{.*}}] value="2"

@section("mysection") let intBinaryArithOp5 = 4 % 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp5")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:22:47 range=[{{.*}}] value="0"

// Bitwise operators on integers
@section("mysection") let intBinaryBitwiseOp1 = 1 & 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp1")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:27:49 range=[{{.*}}] value="0"

@section("mysection") let intBinaryBitwiseOp2 = 1 | 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp2")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:31:49 range=[{{.*}}] value="3"

@section("mysection") let intBinaryBitwiseOp3 = 1 ^ 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp3")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:35:49 range=[{{.*}}] value="3"

@section("mysection") let intBinaryBitwiseOp4 = 1 << 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp4")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:39:49 range=[{{.*}}] value="4"

@section("mysection") let intBinaryBitwiseOp5 = 4 >> 1
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp5")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:43:49 range=[{{.*}}] value="2"

// Unary prefix operators on integers
@section("mysection") let intUnaryOp1: Int = -(1)
// CHECK-LABEL: (pattern_named type="Int" "intUnaryOp1")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:48:46 range=[{{.*}}] negative value="1"

@section("mysection") let intUnaryOp2: Int = +1
// CHECK-LABEL: (pattern_named type="Int" "intUnaryOp2")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperandsAST.swift:52:46 range=[{{.*}}] value="1"

@section("mysection") let intUnaryOp3: UInt8 = ~0b00001111  // equals 0b11110000 (240 in decimal)
// CHECK-LABEL: (pattern_named type="UInt8" "intUnaryOp3")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="UInt8" location={{.*}}LiteralOperandsAST.swift:56:48 range=[{{.*}}] value="240"
