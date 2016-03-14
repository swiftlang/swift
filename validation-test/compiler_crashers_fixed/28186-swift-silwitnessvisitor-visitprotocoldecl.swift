// RUN: not %target-swift-frontend %s -parse
// REQUIRES: objc_interop

// Issue found by https://github.com/PartiallyFinite (Greg Omelaenko)

import Foundation

protocol P {
  static func f() -> Self
  static func g() -> Self
}

extension P {
  static func f() -> P {
    return g()
  }
}

extension NSData: P {
  static func g() -> Self {
    return self.init()
  }
}

