// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules -module-cache-path %t -enable-cxx-interop

import CXXInterop

// Basic structs
do {
  var tmp: Basic = makeA()
  tmp.a = 3
  tmp.b = nil
}
