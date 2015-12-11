// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: objc_interop

// Distributed under the terms of the MIT license
// Test case found by https://github.com/benshan (Ben Shanfelder)

import Foundation

extension NSObject {
    var handler: Int {
        // FIXME: Crashing due to lack of get {}
        set {}
    }
}
