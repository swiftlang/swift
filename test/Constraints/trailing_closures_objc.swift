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
