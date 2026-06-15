// RUN: %target-typecheck-verify-swift -disable-objc-attr-requires-foundation-module -enable-objc-interop

// REQUIRES: objc_interop

// Unicode.Scalar parameters in @objc methods are exposed as 'char32_t' in the
// generated Objective-C header.

@objc class Foo {
  @objc func takesScalar(_ x: Unicode.Scalar) {}
  // expected-warning@-1{{'Unicode.Scalar' parameter 'x' will be exposed as 'char32_t'}}
  // expected-note@-2{{use 'UInt32' and validate with 'Unicode.Scalar.init(_:)'}}

  // Returning Unicode.Scalar is sound (every valid scalar fits in char32_t),
  // so no warning here.
  @objc func returnsScalar() -> Unicode.Scalar { fatalError() }

  // Multiple parameters: warn on each Unicode.Scalar, leave the others alone.
  @objc func mixed(_ a: Int, _ b: Unicode.Scalar, _ c: Unicode.Scalar) -> Unicode.Scalar {}
  // expected-warning@-1{{'Unicode.Scalar' parameter 'b' will be exposed as 'char32_t'}}
  // expected-note@-2{{use 'UInt32' and validate with 'Unicode.Scalar.init(_:)'}}
  // expected-warning@-3{{'Unicode.Scalar' parameter 'c' will be exposed as 'char32_t'}}
  // expected-note@-4{{use 'UInt32' and validate with 'Unicode.Scalar.init(_:)'}}
}

// @objc protocol requirements get the same treatment.
@objc protocol Bar {
  @objc func proto(_ x: Unicode.Scalar)
  // expected-warning@-1{{'Unicode.Scalar' parameter 'x' will be exposed as 'char32_t'}}
  // expected-note@-2{{use 'UInt32' and validate with 'Unicode.Scalar.init(_:)'}}
}
