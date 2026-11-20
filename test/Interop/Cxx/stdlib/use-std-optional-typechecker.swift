// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import StdOptional
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} // expected-note* {{'where T: Copyable' is implicit here}}

func takeCxxOptional<T: CxxOptional>(_ x: T) {
  // The standard library may declare 'optional' in an inline namespace, so
  // only match from 'optional' onwards.
  // expected-note@-3 {{optional<HasDeletedCopyCtor>.Wrapped' (aka 'HasDeletedCopyCtor')}}
  // expected-note@-4 {{'where T: Copyable' is implicit here}}
  _ = x.hasValue
}

let nonNilCopyable = getNonNilOptional()
takeCxxOptional(nonNilCopyable)
takeCopyable(nonNilCopyable)
var _ = nonNilCopyable.pointee // expected-warning {{'pointee' is deprecated: use 'value' instead}}
// expected-note@-1 {{use 'value' instead}}

let nonNilOptNonCopyable = getNonNilOptionalHasDeletedCopyCtor()
takeCopyable(nonNilOptNonCopyable) // expected-error {{conform to 'Copyable'}}
// 'pointee' is not deprecated, since 'value' would have to copy the value.
var _ = nonNilOptNonCopyable.pointee
_ = nonNilOptNonCopyable.hasValue

func takeNonCopyableCxxOptional<T: CxxOptional & ~Copyable>(_ x: borrowing T)
where T.Wrapped: ~Copyable {
  _ = x.hasValue
}
takeNonCopyableCxxOptional(nonNilOptNonCopyable)
// By default, a generic 'CxxOptional' has a copyable 'Wrapped' type.
takeCxxOptional(nonNilOptNonCopyable)
// expected-error@-1 {{optional<HasDeletedCopyCtor>.Wrapped' (aka 'HasDeletedCopyCtor') conform to 'Copyable'}}
// expected-error@-2 {{global function 'takeCxxOptional' requires that 'StdOptionalHasDeletedCopyCtor' (aka 'std.}}

let _: StdOptionalHasDeletedCopyCtor = nil
let _ = StdOptionalHasDeletedCopyCtor(HasDeletedCopyCtor(1))
// The constructor taking 'T &&' is renamed to satisfy 'CxxOptional.init(_:)'.
let _ = StdOptionalHasDeletedCopyCtor(consuming: HasDeletedCopyCtor(1))
// expected-error@-1 {{extraneous argument label 'consuming:' in call}}

// Copyable in C++, but not in Swift.
let annotated = StdOptionalAnnotatedNonCopyable(AnnotatedNonCopyable(1))
takeCopyable(annotated) // expected-error {{conform to 'Copyable'}}
takeNonCopyableCxxOptional(annotated)
var _ = annotated.pointee

let nested = StdOptionalOptionalHasDeletedCopyCtor(
  StdOptionalHasDeletedCopyCtor(HasDeletedCopyCtor(1)))
takeNonCopyableCxxOptional(nested)
_ = nested.pointee.pointee.value

let _ = StdOptionalMoveOnlyVector(makeMoveOnlyVector())

let optVectorNonCopyable = getNonNilOptionalMoveOnlyVector()
takeCopyable(optVectorNonCopyable) // expected-error {{conform to 'Copyable'}}
var _ = optVectorNonCopyable.pointee
var _ = optVectorNonCopyable.pointee[0].value

let _ = returnsConvertsToTemplated() // shouldn't crash the compiler

func testNulloptAssign(_ opt: inout StdOptionalInt) {
  opt = std.nullopt
  // expected-error@-1 {{cannot assign value of type}}
  // expected-note@-2 {{use the 'nil' literal instead}} {{9-20=nil}}
}

func testNulloptInit() {
  let _: StdOptionalInt = std.nullopt
  // expected-error@-1 {{cannot convert value of type}}
  // expected-note@-2 {{use the 'nil' literal instead}} {{27-38=nil}}
}

func testNulloptReturn() -> StdOptionalInt {
  return std.nullopt
  // expected-error@-1 {{cannot convert return expression of type}}
  // expected-note@-2 {{use the 'nil' literal instead}} {{10-21=nil}}
}

func testNulloptDefaultArg(_ x: StdOptionalInt = std.nullopt) {}
// expected-error@-1 {{default argument value of type}}
// expected-note@-2 {{use the 'nil' literal instead}} {{50-61=nil}}

func testNulloptTernary() -> StdOptionalInt {
  return true ? StdOptionalInt(1) : std.nullopt
  // expected-error@-1 {{cannot convert return expression of type}}
  // expected-note@-2 {{use the 'nil' literal instead}}
}
