// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

// https://github.com/swiftlang/swift/issues/54143

import Foundation
import CoreGraphics

class UIViewController {
  var view: UIView
  var preferredContentSize: CGSize

  init() { fatalError() }
}

class UIView {
  var bounds: CGRect
  var frame: CGRect
  var subviews: [UIView] = []

  init() { fatalError() }
}

// This is invalid, because there is no one-argument form of reduce()

class SomeViewController: UIViewController {
    private func updatePreferredContentSize() {
        // expected-error@+1 {{failed to produce diagnostic}}
        preferredContentSize = CGSize(
            width: view.bounds.width,
            height: (view.subviews.map { $0.frame.height }.reduce(+) ?? 0) + 20.0
        )
    }
}
