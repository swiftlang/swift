// RUN: %target-swift-frontend %s -emit-ir
//
// REQUIRES: objc_interop

import Foundation

public class C : NSObject {}

public protocol P {
  associatedtype A : C
}

public class X<T : P> : NSObject {
  public func foo(x: T.A) -> T.A {
    while true {}
  }
}
