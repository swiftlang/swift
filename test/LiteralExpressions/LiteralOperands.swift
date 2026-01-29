// Constant globals using @section initialized with literal expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature LiteralExpressions -verify

// Simple arithmetic operators on integers
@section("mysection") let intBinaryArithOp1 = 1 + 1
@section("mysection") let intBinaryArithOp2 = 4 - 1
@section("mysection") let intBinaryArithOp3 = 3 * 2
@section("mysection") let intBinaryArithOp4 = 4 / 2
@section("mysection") let intBinaryArithOp5 = 4 % 2

// Bitwise operators on integers
@section("mysection") let intBinaryBitwiseOp1 = 1 & 2
@section("mysection") let intBinaryBitwiseOp2 = 1 | 2
@section("mysection") let intBinaryBitwiseOp3 = 1 ^ 2
@section("mysection") let intBinaryBitwiseOp4 = 1 << 2
@section("mysection") let intBinaryBitwiseOp5 = 4 >> 1

// Unary prefix operators on integers
@section("mysection") let intUnaryOp1: Int = -(1)
@section("mysection") let intUnaryOp2: Int = +1
@section("mysection") let intUnaryOp3: UInt8 = ~0b00001111  // equals 0b11110000 (240 in decimal)
