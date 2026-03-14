// RUN: %target-typecheck-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import Foundation

func testReturn1(_ d: Double) -> CGFloat {
  if .random() { d } else { 0 }
}

func testReturn2(_ d: Double) -> CGFloat {
  return if .random() { d } else { 0 }
}

func testReturn3(_ d: Double) -> CGFloat {
  switch Bool.random() { case true: d case false: 0.0 }
}

func testClosure(_ d: CGFloat) {
  func fn(_: () -> Double) {}
  fn {
    if .random() { d } else { 0.0 }
  }
  fn {
    if .random() { 0 } else { d }
  }
  fn {
    if .random() { d } else { d }
  }
  fn {
    return if .random() { d } else { d }
  }
  fn {
    switch Bool.random() {
      case true:
      d
    case false:
      0.0
    }
  }
}

func testAssignment(_ d: CGFloat) -> Double {
  let d1: Double = if .random() { d } else { 0.0 }
  let d2: Double = switch Bool.random() { case true: d case false: 0.0 }
  return .random() ? d1 : d2
}
