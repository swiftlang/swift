// RUN: %target-swift-frontend -o - -emit-silgen %s

// REQUIRES: concurrency

// READ THIS! This test is meant to test invariants around CaptureInfo usage in
// SILGen. Only add examples to this if we hit a crasher in capture info and the
// example makes sure we do not crash again.

class GetIsolatedParamTest {
  public static var rootType: Any.Type? { nil }
}

extension GetIsolatedParamTest : CustomDebugStringConvertible {
    public var debugDescription: String {
      let description = "\\\(String(describing: Self.rootType!))"
      let x: Any.Type? = nil
      // The error is triggered by the ?? autoclosure.
      var valueType: Any.Type? = x ?? Self.rootType
      return description
    }
}
