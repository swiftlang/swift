// RUN: %target-swift-frontend %s -typecheck

// REQUIRES: objc_interop

import Foundation

struct X {
  var a: NSObject
}

[X]()

