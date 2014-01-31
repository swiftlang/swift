// Used by the objc_protocols test to test extensions that add conformances
// using existing methods on a class.

import gizmo

class Bas {
  func runce() -> NSObject { return NSObject() }
}

class Zang {
  func funge() {}
}
