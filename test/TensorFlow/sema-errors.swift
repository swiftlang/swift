// RUN: %target-swift-frontend -O -emit-sil -verify %s

// This file contains tests that produce errors in the semantic analysis pass
// or earlier.  These have to be split from the other SIL tests because if there
// is a type checking error, then the SIL passes aren't run.

import TensorFlow


// Make sure we cleanly diagnose this.
public func testAmbiguous() {
  #tfop("foo")  // expected-error {{type of expression is ambiguous without more context}}
}

