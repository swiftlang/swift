// RUN: rm -rf %t/clang-module-cache
// RUN: %target-build-swift -Xfrontend %clang-importer-sdk -module-cache-path %t/clang-module-cache %s -emit-ir
// XFAIL: linux

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
