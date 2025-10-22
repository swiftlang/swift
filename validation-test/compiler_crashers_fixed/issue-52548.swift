// RUN: not %target-swift-frontend -emit-sil %s
// REQUIRES: OS=ios

import UIKit

// https://github.com/apple/swift/issues/52548
// Just make sure we don't crash.

class Foo: NSObject {
  func crash() {
    let kp = \AnyObject.accessibilityFrame
  }
}
