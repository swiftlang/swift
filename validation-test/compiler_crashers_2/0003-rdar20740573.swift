// RUN: %target-swift-frontend %s -parse

// REQUIRES: objc_interop

import Foundation

struct X {
  var a: NSObject
}

[X]()

