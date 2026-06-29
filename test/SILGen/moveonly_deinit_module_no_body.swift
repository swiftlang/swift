// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-frontend -emit-module -g -experimental-skip-non-inlinable-function-bodies-without-types %s

// Just make sure we don't crash.

public struct S: ~Copyable {
  private let desc: Int

  internal init(desc: Int) {
    self.desc = desc
  }

  deinit {
  }
}
