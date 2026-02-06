// RUN: %target-typecheck-verify-swift -solver-disable-performance-hacks -verify-ignore-unrelated
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

// Invalid expression, because there is no one-argument form of reduce()

class SomeViewController: UIViewController {
    private func updatePreferredContentSize() {
        preferredContentSize = CGSize(
            width: view.bounds.width,
            height: (view.subviews.map { $0.frame.height }
              .reduce(+)
              // expected-error@-1 {{missing argument for parameter #2 in call}}
              ?? 0) + 20.0
        )
    }
}
