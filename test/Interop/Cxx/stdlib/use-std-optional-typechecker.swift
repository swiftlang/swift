// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import StdOptional
import CxxStdlib

func takeCopyable<T: Copyable>(_ x: T) {} // expected-note {{'where T: Copyable' is implicit here}}

let nonNilOptNonCopyable = getNonNilOptionalHasDeletedCopyCtor()
takeCopyable(nonNilOptNonCopyable) // expected-error {{conform to 'Copyable'}}
