// RUN: %target-typecheck-verify-swift

// REQUIRES: rdar66110025
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
  // expected-error@-1 {{cannot convert value of type '(NSIndexPath, NSIndexPath) -> Bool' to expected argument type '(IndexPath, IndexPath) throws -> Bool'}}
}

class Test: NSObject {
  var categories : NSArray?
  func rdar28012273() {
    let categories = ["hello", "world"]
    self.categories = categories.sorted { $0.localizedCaseInsensitiveCompare($1) == ComparisonResult.orderedDescending }
    // expected-error@-1 {{cannot assign value of type '[String]' to type 'NSArray'}} {{121-121= as NSArray}}
  }
}
