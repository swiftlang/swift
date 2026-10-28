// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}function-template-with-optional-frt.h -cxx-interoperability-mode=default -disable-availability-checking

import FunctionTemplateWithOptionalFrt

func testDowncast(base: FRTBase) {
  let _: FRTDerived = downcast(base)
  let _: FRTDerived? = downcast(base)
}

func testNullableDowncast(base: FRTBase, optionalBase: FRTBase?) {
  let _: FRTDerived? = nullableDowncast(base)
  let _: FRTDerived? = nullableDowncast(optionalBase)
  let _: FRTDerived = nullableDowncast(base)!
  if let _: FRTDerived = nullableDowncast(base) {}

  let _: FRTDerived = nullableDowncast(base)
  // expected-error@-1 {{cannot convert value of type 'DerivedPtr?' to specified type 'FRTDerived'}}
  // expected-error@-2 {{generic parameter 'DerivedPtr' could not be inferred}}
}

// A nullable template type parameter cannot be replaced by an optional type:
// the instantiated C++ signature has a single level of nullability.
// TODO: The error is reported at the template declaration in the header.
func testNullableDowncastToDoubleOptional(base: FRTBase) {
  let _: FRTDerived?? = nullableDowncast(base)
}
