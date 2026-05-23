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
