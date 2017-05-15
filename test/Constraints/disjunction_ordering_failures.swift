// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import Foundation

// rdar://problem/32186599
public protocol P { }
extension CGFloat: P { }

func rdar32186599() {
  let h: CGFloat = 0
  let _: P = -(h / 2)
}
