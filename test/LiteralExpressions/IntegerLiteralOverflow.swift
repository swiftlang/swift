// An integer literal whose value does not fit its type is left unfolded, so the
// SIL constant-propagation overflow diagnostic still fires instead of the
// folder silently storing a wrapped value.
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature LiteralExpressions -verify

@section("mysection") let unsignedTooWide: UInt8 = 300
// expected-error@-1 {{integer literal '300' overflows when stored into 'UInt8'}}

@section("mysection") let signedTooWide: Int8 = 200
// expected-error@-1 {{integer literal '200' overflows when stored into 'Int8'}}

// A value that fits still folds.
@section("mysection") let unsignedFits: UInt8 = 255
@section("mysection") let signedFits: Int8 = 127

// A negative literal never fits an unsigned type. 'IntegerLiteralExpr::getValue'
// wraps it before the bit-width check can see it, so negativity is tested first.
@section("mysection") let unsignedNegative: UInt8 = -1
// expected-error@-1 {{negative integer '-1' overflows when stored into unsigned type 'UInt8'}}

// The wrapped value could otherwise collide with another raw value and be
// reported as "raw value for enum case is not unique" instead.
enum NegativeRawValue: UInt8 {
  case a = -1
  // expected-error@-1 2 {{negative integer '-1' overflows when stored into unsigned type 'UInt8'}}
  case b
}
