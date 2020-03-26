// REQUIRES: VENDOR=apple 
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -disable-availability-checking -emit-ir -o /dev/null -module-name opaque_result_type -emit-tbd -emit-tbd-path %t/opaque_result_type.tbd %s -validate-tbd-against-ir=missing

public protocol O {
  func bar()
}
public protocol O2 {
  func baz()
}

public protocol P {
  associatedtype A: O

  func poo() -> A
}
public protocol Q: AnyObject {
  associatedtype B: O, O2

  func qoo() -> B
}

extension Int: O, O2 {
  public func bar() {}
  public func baz() {}
}

public var globalProp: some O {
  return 0
}

public class C: P, Q {
  public func poo() -> some O {
    return 0
  }

  public func qoo() -> some O & O2 {
    return 0
  }

  public var prop: some O {
    return 0
  }

  public subscript() -> some O {
    return 0
  }
}

public func foo(x: String) -> some P {
  return x
}

public func bar(y: C) -> some Q {
  return y
}

public func baz<T: P & Q>(z: T) -> some P & Q {
  return z
}

extension String: P {
  public func poo() -> some O {
    return 0
  }
}

