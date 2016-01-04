// RUN: not --crash %target-swift-frontend %s -parse

// The test requires that NSControl is a class.  Remove this requirement when
// the crash is fixed.
// REQUIRES: OS=macosx

// Distributed under the terms of the MIT license
// Test case found by https://github.com/benshan (Ben Shanfelder)

import Cocoa

protocol A: class {
    var target: AnyObject? { set } // FIXME: Missing "get" wrt extension on NSControl leads to crash.
}

class B: NSObject {
}

extension NSControl: A {}
