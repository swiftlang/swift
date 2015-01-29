// Used by the objc_protocols test to test extensions that add conformances
// using existing methods on a class.

import gizmo

public class Bas {
  @objc public func runce() -> NSObject { return NSObject() }
}

public class Zang {
  @objc public func funge() {}
}
