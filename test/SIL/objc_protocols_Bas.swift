// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs -parse-as-library -parse %s

// Used by the objc_protocols test to test extensions that add conformances
// using existing methods on a class.

import gizmo

class Bas {
  func runce() -> NSObject { return NSObject() }
}
