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
  // Diagnosed twice: the raw literal reaches SIL in both synthesized
  // RawRepresentable members, 'init(rawValue:)' and the 'rawValue' getter.
  // expected-error@-3 2 {{negative integer '-1' overflows when stored into unsigned type 'UInt8'}}
  case b
}

// A signed type's minimum folds. 'APInt::abs()' overflows on it, so the
// magnitude is printed from a widened value and 'setNegative' carries the sign.
@section("mysection") let signedMinimum: Int8 = -128
@section("mysection") let signedBelowMinimum: Int8 = -129
// expected-error@-1 {{integer literal '-129' overflows when stored into 'Int8'}}

enum SignedMinimumRawValue: Int8 {
  case a = -128
  case b
}
