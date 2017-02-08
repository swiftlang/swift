// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend %clang-importer-sdk-nosource -I %t %s -emit-ir
// REQUIRES: executable_test
// REQUIRES: OS=macosx
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
