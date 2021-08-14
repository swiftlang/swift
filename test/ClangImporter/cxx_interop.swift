// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend -typecheck %s -I %S/Inputs/custom-modules

import CXXInterop

// Basic structs
do {
  var tmp: Basic = makeA()
  tmp.a = 3
  tmp.b = nil
}

// Namespace lookup
func namespaceLookup() -> UnsafeMutablePointer<ns.T> {
  var tmp: UnsafeMutablePointer<ns.T> = ns.doMakeT()!
  return tmp
}
