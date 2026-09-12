// RUN: %target-typecheck-verify-swift -I %S%{fs-sep}Inputs -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}function-template-nullability.h -cxx-interoperability-mode=default

import FunctionTemplateNullability

func testPointers(p: UnsafeMutablePointer<CInt>, cp: UnsafePointer<CInt>) {
  let _: UnsafeMutablePointer<CInt>? = nullableResult(p)
  let _: UnsafeMutablePointer<CInt> = nonnullResult(p)
  let _: UnsafeMutablePointer<CInt> = unspecifiedResult(p)
  let _: UnsafeMutablePointer<CInt>? = unspecifiedResult(p)
  let _: UnsafeMutablePointer<CInt>? = nullableParameter(p)
  let _: UnsafeMutablePointer<CInt>? = nullablePointerResult(p)
  let _: UnsafePointer<CInt>? = nullableConstPointerResult(cp)
  let _: UnsafeMutablePointer<CInt> = nonnullPointerResult(p)

  let _: UnsafeMutablePointer<CInt> = nullableResult(p)
  // expected-error@-1 {{cannot convert value of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to specified type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
  let _: UnsafeMutablePointer<CInt> = nullablePointerResult(p)
  // expected-error@-1 {{value of optional type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') must be unwrapped to a value of type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
  // expected-note@-2 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
  // expected-note@-3 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
}

// A template type parameter declared with a nullability specifier cannot be
// replaced by an optional type.
// TODO: The error is reported in the header, we should emit it at the call
// site.
func testOptionalReplacement(optionalPointer: UnsafeMutablePointer<CInt>?) {
  let _ = nullableResult(optionalPointer)
}
