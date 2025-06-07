// RUN: %target-typecheck-verify-swift -enable-experimental-feature ImplicitLastExprResults %clang-importer-sdk

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImplicitLastExprResults

import Foundation

func testReturn1(_ d: Double) -> CGFloat {
  print("hello")
  d
}

func testReturn2(_ d: Double) -> CGFloat {
  ()
  if .random() { d } else { 0 }
}

func testReturn3(_ d: CGFloat) -> Double {
  print("hello")
  switch Bool.random() { case true: d case false: 0 }
}

func testReturn4(_ d: Double) -> CGFloat {
  print("hello")
  if .random() {
    print("hello")
    d
  } else {
    if .random() {
      print("hello")
      d
    } else {
      0
    }
  }
}

func testClosure(_ d: CGFloat) {
  func fn(_: () -> Double) {}
  fn {
    print("hello")
    d
  }
  fn {
    print("hello")
    if .random() { 0.0 } else { d }
  }
  fn {
    print("hello")
    if .random() {
      print("hello")
      d
    } else {
      if .random() {
        print("hello")
        d
      } else {
        0
      }
    }
  }
}
