// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -experimental-lazy-typecheck -emit-tbd -emit-tbd-path %t/lazy.tbd %s -enable-library-evolution -parse-as-library

public protocol P {
  func req()
}

// FIXME: This malformed conformance should probably be diagnosed.
public struct S: P {
}
