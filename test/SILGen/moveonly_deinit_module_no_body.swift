// RUN: %target-swift-frontend -emit-module -g -enable-experimental-move-only -experimental-skip-non-inlinable-function-bodies-without-types %s 

// Just make sure we don't crash.

@_moveOnly
public struct S {
  private let desc: Int

  internal init(desc: Int) {
    self.desc = desc
  }

  deinit {
  }
}
