// RUN: %target-swift-frontend %s -typecheck -verify -solver-scope-threshold=1000
// REQUIRES: objc_interop

import Foundation
import CoreGraphics

// https://github.com/swiftlang/swift/issues/52532

func slow() {
  let itemsPerRow = 10
  let size: CGFloat = 20
  let margin: CGFloat = 10
  let _ = (0..<100)
    .map { (row: CGFloat($0 / itemsPerRow), col: CGFloat($0 % itemsPerRow)) }
    .map {
      CGRect(x: $0.col * (size + margin) + margin,
             y: $0.row * (size + margin) + margin,
             width: size,
             height: size)
  }
}
