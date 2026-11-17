// C++ lifetime annotations on the declarations `@cxx @implementation`
// functions implement.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation
// REQUIRES: swift_feature_Lifetimes

import Lifetimes


// A lifetimebound `const T &` parameter imports as `borrowing`, so the
// implementation spells it `borrowing` too, and a non-escapable result depends
// on it.

@unsafe @cxx @implementation
@_lifetime(borrow t)
func viewTracked(_ t: borrowing Tracked) -> TrackedView {
  return TrackedView(value: nil)
}
