// RUN: %target-swift-frontend -typecheck %s -solver-disable-promote-supertypes
// RUN: %target-typecheck-verify-swift -solver-enable-promote-supertypes
// REQUIRES: objc_interop

import Foundation

protocol Shape {
  func fill(_: Color)
}

struct Color {
  func opacity(_: Double) -> Color {
    self
  }
}

// While SE-0307 says this should be rejected, it was accepted because of a bug.

func test(shape: any Shape, color: Color, x: Double) {
  _ = shape.fill(color.opacity(CGFloat(x)))
  // expected-error@-1 {{cannot convert value of type 'CGFloat' to expected argument type 'Double'}}
}
