// RUN: not %target-swift-frontend -emit-sil %s
// REQUIRES: OS=ios

import UIKit

// Just make sure we don't crash.

class Foo: NSObject {
  func crash() {
    let kp = \AnyObject.accessibilityFrame
  }
}
