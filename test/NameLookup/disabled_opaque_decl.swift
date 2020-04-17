// Ensure scope construction does not crash
// RUN: %target-swift-frontend -emit-module %s

public protocol P { }

extension Int: P { }

#if false
public struct Foo {
  public func getP() -> some P {
    return 17
  }
}
#endif
