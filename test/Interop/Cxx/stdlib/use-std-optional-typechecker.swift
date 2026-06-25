// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import StdOptional
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} // expected-note {{'where T: Copyable' is implicit here}}

func takeCxxOptional<T: CxxOptional>(_ x: T) {
  _ = x.hasValue
}

let nonNilCopyable = getNonNilOptional()
takeCxxOptional(nonNilCopyable)
takeCopyable(nonNilCopyable)
var _ = nonNilCopyable.pointee // expected-warning {{'pointee' is deprecated: use 'value' instead}}
// expected-note@-1 {{use 'value' instead}}

let nonNilOptNonCopyable = getNonNilOptionalHasDeletedCopyCtor()
takeCopyable(nonNilOptNonCopyable) // expected-error {{conform to 'Copyable'}}
var _ = nonNilOptNonCopyable.pointee

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
