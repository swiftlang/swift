// RUN: not %target-swift-frontend %s -parse

// Issue found by https://github.com/benshan (Ben Shanfelder)

import Cocoa

protocol A: class {
    var target: AnyObject? { set } // FIXME: Missing "get" wrt extension on NSControl leads to crash.
}

class B: NSObject {
}

extension NSControl: A {}
