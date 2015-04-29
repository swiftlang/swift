// RUN: %target-swift-frontend %s -parse

import Foundation

struct X {
  var a: NSObject
}

[X]()

