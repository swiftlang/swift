// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -verify -experimental-lazy-typecheck -emit-tbd -emit-tbd-path %t/lazy.tbd %s -enable-library-evolution -parse-as-library -tbd-install_name lazy

public protocol P {
  func req()
}

// expected-error@+1 {{type 'S' does not conform to protocol 'P'}}
public struct S: P {
}

extension DoesNotExist {
  public func method() {}
}
