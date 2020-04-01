// RUN: %target-typecheck-verify-swift -enable-objc-interop
// REQUIRES: OS=macosx

import Foundation

// Can downcast by bridging
let bridge = "A" as? NSString // expected-warning {{always succeeds}}
let bridge1 = 1 as? NSNumber // expected-warning {{always succeeds}}
