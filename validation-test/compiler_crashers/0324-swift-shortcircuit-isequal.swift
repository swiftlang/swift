// RUN: rm -rf %t/clang-module-cache
// RUN: not --crash %target-swift-frontend %s -module-cache-path %t/clang-module-cache -emit-silgen

// Distributed under the terms of the MIT license
// Test case submitted to project by http://github.com/martijnwalraven (Martijn Walraven)
// rdar://18662915

import Foundation
class Test: NSObject {
  override func isEqual(object: AnyObject?) -> Bool {
    return false && super.isEqual(object)
  }
}
