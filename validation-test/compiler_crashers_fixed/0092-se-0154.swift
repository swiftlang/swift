// RUN: not %target-swift-frontend %s -typecheck
// REQUIRES: objc_interop

import Foundation
let nsd: NSDictionary = [NSObject : AnyObject]()

