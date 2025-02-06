// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50

// REQUIRES: OS=macosx,no_asan
// REQUIRES: objc_interop

import Foundation

struct Size {
  var width: CGFloat
  var height: CGFloat
}

func frame(width: CGFloat?, height: CGFloat?) {}

func test(size: Size?) {
  frame(width: ((size?.width ?? 0) * 1) + 1.0, height: ((size?.height ?? 0) * 1) + 1.0)
}
