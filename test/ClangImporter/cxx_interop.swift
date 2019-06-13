// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -I %S/Inputs/custom-modules -module-cache-path %t -enable-cxx-interop
// UNSUPPORTED: macosx

import CXXInterop

// Basic structs
do {
  var tmp: ns_Basic = makeA()
  tmp.a = 3
}

var tmp: NestedStruct = MakeNestedStruct()
