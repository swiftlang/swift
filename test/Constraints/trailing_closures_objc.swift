// RUN: %target-swift-frontend -typecheck -verify %s

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation
import AVFoundation
import AppKit

func foo(options: [AVMediaSelectionOption]) {
  let menuItems: [NSMenuItem] = options.map { (option: AVMediaSelectionOption) in
    NSMenuItem(title: option.displayName, action: #selector(NSViewController.respondToMediaOptionSelection(from:)), keyEquivalent: "")
    // expected-error@-1 {{type 'NSViewController' has no member 'respondToMediaOptionSelection(from:)'}}
  }
}

func rdar28004686(a: [IndexPath]) {
  _ = a.sorted { (lhs: NSIndexPath, rhs: NSIndexPath) -> Bool in true }
  // expected-error@-1 {{'NSIndexPath' is not convertible to 'IndexPath'}}
}
