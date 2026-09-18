// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default \
// RUN:   -DUSE_RENAMED_FROM -verify-additional-prefix from- \
// RUN:   -I %S%{fs-sep}Inputs \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}swift-name-different-type.h

// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default \
// RUN:   -DUSE_RENAMED_TO -verify-additional-prefix to- \
// RUN:   -I %S%{fs-sep}Inputs \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}swift-name-different-type.h

import SwiftNameDifferentType

func test(_ a: A, _ b: B) {

#if USE_RENAMED_FROM
  a.renamedFrom0()   // expected-from-error {{has no member}}
  a.renamedFrom1(42) // expected-from-error {{has no member}}
#endif

#if USE_RENAMED_TO
  b.renamedTo0()    // expected-to-error {{has no member}}
  b.renamedTo1(42)  // expected-to-error {{has no member}}
#endif

  b.other()
}
