// RUN: %target-swift-frontend %s -typecheck -verify -solver-scope-threshold=100
// REQUIRES: objc_interop

// rdar://173654813
// REQUIRES: OS=macosx

// https://github.com/swiftlang/swift/issues/52629

import Foundation

func slow() {
  let padding: CGFloat = 1
  let stroke: CGFloat = 15
  let origin: CGFloat = 2
  let handleViewHeight: CGFloat = 1

  _ = CGPoint(x: 1, y: (origin + (padding + handleViewHeight/2 + stroke/2))/1.75)
}
