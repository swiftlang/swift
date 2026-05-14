// Constant globals using @section initialized with literal expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -typecheck -dump-ast %s -enable-experimental-feature LiteralExpressions -verify | %FileCheck %s

// Simple arithmetic operators on integers
@section("mysection") let intBinaryArithOp1 = 1 + 1
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp1")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="2"

@section("mysection") let intBinaryArithOp2 = 4 - 1
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp2")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="3"

@section("mysection") let intBinaryArithOp3 = 3 * 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp3")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="6"

@section("mysection") let intBinaryArithOp4 = 4 / 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp4")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="2"

@section("mysection") let intBinaryArithOp5 = 4 % 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryArithOp5")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="0"

// Bitwise operators on integers
@section("mysection") let intBinaryBitwiseOp1 = 1 & 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp1")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="0"

@section("mysection") let intBinaryBitwiseOp2 = 1 | 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp2")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="3"

@section("mysection") let intBinaryBitwiseOp3 = 1 ^ 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp3")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="3"

@section("mysection") let intBinaryBitwiseOp4 = 1 << 2
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp4")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="4"

@section("mysection") let intBinaryBitwiseOp5 = 4 >> 1
// CHECK-LABEL: (pattern_named type="Int" "intBinaryBitwiseOp5")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="2"

// Signed right shift is arithmetic, preserving the sign bit.
@section("mysection") let intBinaryShiftRightSigned: Int8 = -8 >> 1
// CHECK-LABEL: (pattern_named type="Int8" "intBinaryShiftRightSigned")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int8" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] negative value="4"

// Unsigned right shift remains logical.
@section("mysection") let intBinaryShiftRightUnsigned: UInt8 = 128 >> 1
// CHECK-LABEL: (pattern_named type="UInt8" "intBinaryShiftRightUnsigned")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="UInt8" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="64"

// Overflow arithmetic operators on integers
@section("mysection") let intOverflowAddUnsigned: UInt8 = 250 &+ 10  // wraps: 260 mod 256
// CHECK-LABEL: (pattern_named type="UInt8" "intOverflowAddUnsigned")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="UInt8" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="4"

@section("mysection") let intOverflowSubSigned: Int8 = -128 &- 1  // wraps: -129 -> 127
// CHECK-LABEL: (pattern_named type="Int8" "intOverflowSubSigned")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int8" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="127"

@section("mysection") let intOverflowMulSigned: Int8 = 100 &* 2  // wraps: 200 -> -56
// CHECK-LABEL: (pattern_named type="Int8" "intOverflowMulSigned")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int8" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] negative value="56"

// Masking shift operators on integers
@section("mysection") let intMaskingShiftLeftOverflow: UInt8 = 1 &<< 9  // amount masks to 1
// CHECK-LABEL: (pattern_named type="UInt8" "intMaskingShiftLeftOverflow")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="UInt8" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="2"

@section("mysection") let intMaskingShiftRightUnsigned: UInt8 = 1 &>> 9  // amount masks to 1
// CHECK-LABEL: (pattern_named type="UInt8" "intMaskingShiftRightUnsigned")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="UInt8" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="0"

@section("mysection") let intMaskingShiftRightSigned: Int8 = -8 &>> 1  // arithmetic shift
// CHECK-LABEL: (pattern_named type="Int8" "intMaskingShiftRightSigned")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int8" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] negative value="4"

// Unary prefix operators on integers
@section("mysection") let intUnaryOp1: Int = -(1)
// CHECK-LABEL: (pattern_named type="Int" "intUnaryOp1")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] negative value="1"

@section("mysection") let intUnaryOp2: Int = +1
// CHECK-LABEL: (pattern_named type="Int" "intUnaryOp2")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="Int" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="1"

@section("mysection") let intUnaryOp3: UInt8 = ~0b00001111  // equals 0b11110000 (240 in decimal)
// CHECK-LABEL: (pattern_named type="UInt8" "intUnaryOp3")
// CHECK: (processed_constant_folded_init=integer_literal_expr implicit type="UInt8" location={{.*}}LiteralOperands.swift:{{[0-9]+}}:{{[0-9]+}} range=[{{.*}}] value="240"
