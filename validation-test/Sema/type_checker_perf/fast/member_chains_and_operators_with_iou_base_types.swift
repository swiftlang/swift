// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -solver-expression-time-threshold=1

// REQUIRES: OS=macosx,no_asan
// REQUIRES: objc_interop

import Foundation

struct CGRect {
  var x: CGFloat

  init(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat) { }
  init(x: Double, y: Double, width: Double, height: Double) { }
}

protocol View {}

extension Optional: View where Wrapped: View {}

extension View {
  func frame() -> some View { self }
  func frame(x: Int, y: Int, w: Int, z: Int) -> some View { self }
  func frame(y: Bool) -> some View { self }
}

struct NSView {
  var frame: CGRect
}

func test(margin: CGFloat, view: NSView!) -> CGRect {
  // `view` is first attempted as `NSView?` and only if that fails is force unwrapped
  return CGRect(x: view.frame.x + margin,
                y: view.frame.x + margin,
                width: view.frame.x - view.frame.x - view.frame.x - (margin * 2),
                height: margin)
}
