// Constant globals using @section initialized with literal expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature LiteralExpressions -verify

@section("mysection") let overflow: UInt8 = 100 * 3 // expected-error {{operation results in integer overflow}}
@section("mysection") let divideZero1 = 4 / 0 // expected-error {{division by zero}}
@section("mysection") let divideZero2 = 4 % 0 // expected-error {{division by zero}}
