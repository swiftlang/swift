// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50
// REQUIRES: OS=macosx

import AppKit

// https://github.com/swiftlang/swift/issues/45731

class Test {
  var fontWeight: NSFont.Weight = .regular

  @IBOutlet weak var a1Label: NSTextField!

  func slow() {
    for label in [a1Label, a1Label, a1Label, a1Label, a1Label, a1Label, a1Label, a1Label, a1Label, a1Label, a1Label, a1Label] {
      label!.font = NSFont.monospacedDigitSystemFont(ofSize: label!.font!.pointSize, weight: fontWeight)
    }
  }
}
