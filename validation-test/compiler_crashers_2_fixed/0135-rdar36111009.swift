// RUN: %target-swift-frontend -primary-file %s -emit-ir -o /dev/null

// REQUIRES: objc_interop

import Foundation

@objc protocol P {
    var property: Int64 { get }
}

class PUser<T: P> where T: NSObject {
  init(t: T, updating progress: Progress) {
    _ = t.observe(\.property) { t, _ in
      let _ = t.property
    }
  }
}
