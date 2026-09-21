// RUN: %target-typecheck-verify-swift -import-objc-header %S/Inputs/objc_bad_superclass_corner_case.h
// REQUIRES: objc_interop

import Foundation

class D<T: Hashable>: C {}

struct S {}

// expected-error@+1 {{type 'S' does not conform to protocol 'Hashable'}}
class E: D<S> {
  override func f() {}
}
