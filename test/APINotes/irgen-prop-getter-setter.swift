// Also run this test in optimize test modes.
// REQUIRES: optimize_test

// RUN: %target-build-swift -Xfrontend %clang-importer-sdk %s -emit-ir

// REQUIRES: objc_interop

// Test that we don't crash when producing IR.

import AppKit
class MyView: NSView {
    func drawRect() {
        var x = self.superview
        var l = self.layer
        self.layer = CALayer()
        self.nextKeyView = nil
        subviews = []
    }    
}
var m = MyView()
m.drawRect()
