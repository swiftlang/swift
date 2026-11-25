// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %S/Inputs -cxx-interoperability-mode=default

// Members of the Cxx overlay that would have to copy the wrapped value of a
// 'std::optional' are not available if it is non-copyable. The diagnostics
// point into the Cxx overlay, hence '-verify-ignore-unrelated'.

import StdOptional
import CxxStdlib

let opt = getNonNilOptionalHasDeletedCopyCtor()

_ = Optional(fromCxx: opt)
// expected-error@-1 {{referencing initializer 'init(fromCxx:)' on 'Optional' requires that 'std.}}
