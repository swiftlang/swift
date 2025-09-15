// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

// Note this cannot use a fake Foundation because it lacks required operator overloads

import Foundation
import CoreGraphics

do {
  func test(a: CGFloat, b: CGFloat) {
    let _: Double = a + b + 1 // Ok
    let _: Double = a + b + 1.0 // Ok

    var test: Double

    test = a + b + 1 // Ok
    test = a + b + 1.0 // Ok

    _ = test

    let _ = a + b + 1 // Ok
    let _ = a + b + 1.0 // Ok

    let _: Double = 1 + 2 + 3 // Ok

    test = 1 + 2 + 3 // Ok
  }
}

func test(cond: Bool, a: CGFloat, b: CGFloat) {
  var test: Double

  let width = a - b // CGFloat

  if cond {
    test = (width - 10) / 2 // Ok
  } else {
    test = (width - 20.0) / 3 // Ok
  }

  _ = test
}

func test_atan_ambiguity(points: (CGPoint, CGPoint)) {
  var test = 0.0
  test = atan((points.1.y - points.0.y) / (points.1.x - points.0.x)) // Ok
  _ = test
}

func test_ambigity_with_generic_funcs(a: CGFloat, b: CGFloat) -> [CGFloat] {
  let result = [round(abs(a - b) * 100) / 100.0]
  return result
}
