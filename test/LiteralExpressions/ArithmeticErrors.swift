// Constant globals using @section initialized with literal expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature LiteralExpressions -verify

@section("mysection") let overflow: UInt8 = 100 * 3 // expected-error {{operation results in integer overflow}}
@section("mysection") let divideZero1 = 4 / 0 // expected-error {{division by zero}}
@section("mysection") let divideZero2 = 4 % 0 // expected-error {{division by zero}}

// Overflow arithmetic operators wrap silently, without triggering overflow diagnostics.
@section("mysection") let overflowAddWraps: UInt8 = 255 &+ 1
@section("mysection") let overflowSubWraps: UInt8 = 0 &- 1
@section("mysection") let overflowMulWraps: UInt8 = 128 &* 2

// Non-masking shifts reject negative and out-of-range amounts.
@section("mysection") let shiftNegative: Int8 = 1 >> -1
// expected-error@-1 {{negative shift amount is not permitted in a literal expression}}

@section("mysection") let shiftOutOfRange: UInt8 = 1 >> 8
// expected-error@-1 {{shift amount 8 is greater than or equal to the 8-bit width of the operand in a literal expression}}

@section("mysection") let shiftFarOutOfRange: UInt8 = 1 << 64
// expected-error@-1 {{shift amount 64 is greater than or equal to the 8-bit width of the operand in a literal expression}}
